use crate::CompletedMarker;

use super::*;

fn op_bp(ident: &str) -> (u8, u8) {
  // See https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html
  let precedence = match ident {
    s if s.chars().all(|c| c.is_ascii_alphabetic() || c == '_') => 10,
    "|" => 9,
    "^" => 8,
    "&" => 7,
    "=" | "!" => 6,
    "<" | ">" => 5,
    ":" => 4,
    "+" | "-" => 3,
    "*" | "/" | "%" => 2,
    _ => 1,
  };

  if ident.ends_with(":") {
    // Right-associative
    (precedence, precedence)
  } else {
    // Left-associative
    (precedence, precedence + 1)
  }
}

pub fn expr(p: &mut Parser) { expr_bp(p, 0); }

fn expr_bp(p: &mut Parser, min_bp: u8) {
  let m = p.start();

  let Some(mut lhs) = simple_expr(p) else {
    m.abandon(p);
    return;
  };

  loop {
    match p.current() {
      T![ident] => {
        let op = p.slice();
        let (l_bp, r_bp) = op_bp(op);
        if l_bp < min_bp {
          m.abandon(p);
          return;
        }

        let m = lhs.precede(p);
        p.eat(T![ident]);
        expr_bp(p, r_bp);
        lhs = m.complete(p, INFIX_EXPR);
      }

      T![nl] | T![,] | T![')'] | EOF => {
        m.abandon(p);
        return;
      }

      _ => {
        m.abandon(p);
        p.error(format!("expected expression, got {:?}", p.current()));
        p.recover_until_any(&[T![nl], T![,], T![')']]);
        return;
      }
    }
  }
}

// Expression like `hello(3, 4)`
fn simple_expr(p: &mut Parser) -> Option<CompletedMarker> {
  let lhs = atom_expr(p)?;
  let m = postfix_expr(p, lhs);

  Some(m)
}

fn postfix_expr(p: &mut Parser, mut lhs: CompletedMarker) -> CompletedMarker {
  loop {
    lhs = match p.current() {
      // test ok
      // hi(3)
      T!['('] => call_paren_expr(p, lhs),
      T!['{'] => call_block_expr(p, lhs),
      // TODO
      // T!['['] => index_expr(p, lhs),
      T![.] => match postfix_dot_expr(p, lhs) {
        Ok(it) => it,
        Err(it) => {
          lhs = it;
          break;
        }
      },
      _ => break,
    };
  }
  lhs
}

fn call_paren_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
  let m = lhs.precede(p);

  {
    let m = p.start();
    p.eat(T!['(']);

    loop {
      expr(p);
      if p.at(T![,]) {
        p.bump();
      } else {
        break;
      }
    }

    p.expect(T![')']);
    m.complete(p, PAREN_ARGUMENTS);
  }

  m.complete(p, CALL_EXPR)
}

fn call_block_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
  let m = lhs.precede(p);
  p.eat(T!['{']);

  // TODO: This is wrong.
  loop {
    expr(p);
    if p.at(T![,]) {
      p.bump();
    } else {
      break;
    }
  }

  p.expect(T!['}']);

  m.complete(p, CALL_EXPR)
}

fn postfix_dot_expr(
  p: &mut Parser,
  lhs: CompletedMarker,
) -> Result<CompletedMarker, CompletedMarker> {
  let m = lhs.precede(p);
  p.eat(T![.]);
  match p.current() {
    T![ident] => {
      p.bump();
      Ok(m.complete(p, FIELD_EXPR))
    }
    _ => {
      // TODO: need peek() to check for this error before calling `precede`.
      Err(m.complete(p, FIELD_EXPR))
    }
  }
}

// Expressions like `1` or "hi"
fn atom_expr(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  match p.current() {
    INT_LIT_KW => {
      p.eat(INT_LIT_KW);
      Some(m.complete(p, LIT_EXPR))
    }
    STRING_LIT_KW => {
      p.eat(STRING_LIT_KW);
      Some(m.complete(p, LIT_EXPR))
    }
    IDENT => {
      p.eat(IDENT);
      Some(m.complete(p, IDENT))
    }

    _ => {
      m.abandon(p);
      p.error(format!("expected expression, got {:?}", p.current()));
      p.recover_until(T![nl]);
      None
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::tests::check_expr;

  #[test]
  fn literals() {
    check_expr(
      "2",
      expect![@r#"
        LIT_EXPR
          INT_LIT_KW '2'
      "#],
    );

    check_expr(
      "\"hi\"",
      expect![@r#"
        LIT_EXPR
          STRING_LIT_KW '"hi"'
      "#],
    );
  }

  #[test]
  fn whitespace() {
    check_expr(
      "2   +  56",
      expect![@r#"
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '2'
          WHITESPACE '   '
          IDENT '+'
          WHITESPACE '  '
          LIT_EXPR
            INT_LIT_KW '56'
      "#],
    );
  }

  #[test]
  fn binary_op() {
    check_expr(
      "1 + 2",
      expect![@r#"
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '1'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '2'
      "#],
    );

    check_expr(
      "\"hi\" + \"there\"",
      expect![@r#"
        INFIX_EXPR
          LIT_EXPR
            STRING_LIT_KW '"hi"'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            STRING_LIT_KW '"there"'
      "#],
    );
  }

  #[test]
  fn complex_op() {
    check_expr(
      "2 + 2",
      expect![@r#"
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '2'
      "#],
    );

    check_expr(
      "2 + 2 == 5",
      expect![@r#"
        INFIX_EXPR
          INFIX_EXPR
            LIT_EXPR
              INT_LIT_KW '2'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '=='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '5'
      "#],
    );

    check_expr(
      "2 == 2 + 5",
      expect![@r#"
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '=='
          WHITESPACE ' '
          INFIX_EXPR
            LIT_EXPR
              INT_LIT_KW '2'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '5'
      "#],
    );
  }

  #[test]
  fn associativity() {
    check_expr(
      "2 + 3 + 4",
      expect![@r#"
        INFIX_EXPR
          INFIX_EXPR
            LIT_EXPR
              INT_LIT_KW '2'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '4'
      "#],
    );

    // All ops ending in `:` are right-associative
    check_expr(
      "2 +: 3 +: 4",
      expect![@r#"
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '+:'
          WHITESPACE ' '
          INFIX_EXPR
            LIT_EXPR
              INT_LIT_KW '3'
            WHITESPACE ' '
            IDENT '+:'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '4'
      "#],
    );
  }

  #[test]
  fn call_op() {
    check_expr(
      "hi(2)",
      expect![@r#"
        CALL_EXPR
          IDENT
            IDENT 'hi'
          PAREN_ARGUMENTS
            OPEN_PAREN '('
            LIT_EXPR
              INT_LIT_KW '2'
            CLOSE_PAREN ')'
      "#],
    );

    check_expr(
      "hi(2, 3)",
      expect![@r#"
        CALL_EXPR
          IDENT
            IDENT 'hi'
          PAREN_ARGUMENTS
            OPEN_PAREN '('
            LIT_EXPR
              INT_LIT_KW '2'
            COMMA ','
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
            CLOSE_PAREN ')'
      "#],
    );
  }
}
