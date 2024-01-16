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
        dbg!(l_bp, r_bp, op);
        if l_bp < min_bp {
          println!("l_bp < min_bp, abandoning");
          m.abandon(p);
          return;
        }

        let m = lhs.precede(p);
        p.eat(T![ident]);
        expr_bp(p, r_bp);
        lhs = m.complete(p, INFIX_EXPR);
      }

      T![nl] => {
        m.abandon(p);
        return;
      }

      _ => {
        m.abandon(p);
        p.error(format!("expected expression, got {:?}", p.current()));
        p.recover_until(T![nl]);
        return;
      }
    }
  }
}

fn simple_expr(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  match p.current() {
    INT_LIT_KW => {
      p.eat(INT_LIT_KW);
      Some(m.complete(p, LITERAL))
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
        LITERAL
          INT_LIT_KW '2'
      "#],
    );

    // TODO
    // check_expr("\"hi\"", r"LITERAL");
  }

  #[test]
  fn binary_op() {
    check_expr(
      "1 + 2",
      expect![@r#"
        INFIX_EXPR
          LITERAL
            INT_LIT_KW '1'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LITERAL
            INT_LIT_KW '2'
      "#],
    );
  }

  #[test]
  fn complex_op() {
    check_expr(
      "2 + 2",
      expect![@r#"
        INFIX_EXPR
          LITERAL
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LITERAL
            INT_LIT_KW '2'
      "#],
    );

    check_expr(
      "2 + 2 == 5",
      expect![@r#"
        INFIX_EXPR
          INFIX_EXPR
            LITERAL
              INT_LIT_KW '2'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LITERAL
              INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '=='
          WHITESPACE ' '
          LITERAL
            INT_LIT_KW '5'
      "#],
    );

    check_expr(
      "2 == 2 + 5",
      expect![@r#"
        INFIX_EXPR
          LITERAL
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '=='
          WHITESPACE ' '
          INFIX_EXPR
            LITERAL
              INT_LIT_KW '2'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LITERAL
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
            LITERAL
              INT_LIT_KW '2'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LITERAL
              INT_LIT_KW '3'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LITERAL
            INT_LIT_KW '4'
      "#],
    );

    // All ops ending in `:` are right-associative
    check_expr(
      "2 +: 3 +: 4",
      expect![@r#"
        INFIX_EXPR
          LITERAL
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '+:'
          WHITESPACE ' '
          INFIX_EXPR
            LITERAL
              INT_LIT_KW '3'
            WHITESPACE ' '
            IDENT '+:'
            WHITESPACE ' '
            LITERAL
              INT_LIT_KW '4'
      "#],
    );
  }
}
