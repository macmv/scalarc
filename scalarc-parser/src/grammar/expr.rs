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
  let Some(mut lhs) = simple_expr(p) else {
    return;
  };

  loop {
    match p.current() {
      T![ident] | T![=>] => {
        let op = p.slice();
        let (l_bp, r_bp) = op_bp(op);
        if l_bp < min_bp {
          return;
        }

        let m = lhs.precede(p);
        p.bump(); // eat T![ident] or T![=>]

        // this is one expression
        // 2 +
        //   3
        //
        // these are two expressions
        // 2 +
        //
        //   3
        if p.eat_newlines() >= 2 {
          m.abandon(p);
          p.error(format!("expected expression, got expression separator {:?}", p.current()));
          p.recover_until_any(&[T![nl], T![,], T![')'], T!['}']]);
          return;
        };

        expr_bp(p, r_bp);
        lhs = m.complete(p, INFIX_EXPR);
      }

      T![nl] | T![,] | T![')'] | T!['}'] | EOF => {
        return;
      }

      _ => {
        p.error(format!("expected expression, got {:?}", p.current()));
        p.recover_until_any(&[T![nl], T![,], T![')'], T!['}']]);
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
      T!['('] => {
        let call = lhs.precede(p);

        let m = p.start();
        call_paren_expr(p);
        m.complete(p, PAREN_ARGUMENTS);

        call.complete(p, CALL_EXPR)
      }
      // test ok
      // hi { 3 }
      T!['{'] => {
        let m = lhs.precede(p);
        call_block_expr(p);
        m.complete(p, CALL_EXPR)
      }
      // test ok
      // foo.bar
      T![.] => match postfix_dot_expr(p, lhs) {
        Ok(it) => it,
        Err(it) => {
          lhs = it;
          break;
        }
      },

      // test ok
      // foo
      //   .bar
      //   .baz
      T![nl] => match p.peek() {
        T![.] => {
          p.eat(T![nl]);
          match postfix_dot_expr(p, lhs) {
            Ok(it) => it,
            Err(it) => {
              lhs = it;
              break;
            }
          }
        }
        _ => break,
      },

      T![match] => match_expr(p, lhs),

      _ => break,
    };
  }
  lhs
}

// test ok
// 2 match {
//   case 3 => 4
// }
fn match_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
  let m = lhs.precede(p);

  p.eat(T![match]);

  p.expect(T!['{']);
  p.eat_newlines();

  if p.at(T!['}']) {
    p.error_bump("unexpected end of match block");
    return m.complete(p, MATCH_EXPR);
  }

  loop {
    if p.at(EOF) {
      p.error("unexpected end of file");
      break;
    }

    let c = p.start();

    // case_item eats a newline and handles errors.
    super::item::case_item(p, c);

    if p.at(T!['}']) {
      p.eat(T!['}']);
      break;
    }
  }

  m.complete(p, MATCH_EXPR)
}

fn call_paren_expr(p: &mut Parser) {
  p.eat(T!['(']);
  // test ok
  // hi(
  //   3,
  //   4
  // )
  p.eat_newlines();

  // test ok
  // hi()
  if p.at(T![')']) {
    p.eat(T![')']);
    return;
  }

  loop {
    // FIXME: I think this might run into ambiguity with assignments?.
    if p.current() == T![ident] && p.peek() == T![=] {
      // test ok
      // hi(
      //   foo = 3,
      //   bar = 4
      // )
      p.expect(T![ident]);
      p.expect(T![=]);
      expr(p);
    } else {
      expr(p);
    }

    // test ok
    // hi(
    //   3
    //   ,
    //   4
    // )
    p.eat_newlines();
    if p.at(T![,]) {
      p.bump();
      p.eat_newlines();

      // test ok
      // hi(3,4,)
      if p.at(T![')']) {
        break;
      }
    } else {
      break;
    }
  }

  p.expect(T![')']);
}

fn tuple_expr(p: &mut Parser) {
  p.eat(T!['(']);
  // test ok
  // (
  //   3,
  //   4
  // )
  p.eat_newlines();

  // test ok
  // ()
  if p.at(T![')']) {
    p.eat(T![')']);
    return;
  }

  loop {
    expr(p);
    // test ok
    // (
    //   3
    //   ,
    //   4
    // )
    p.eat_newlines();
    if p.at(T![,]) {
      p.bump();
      p.eat_newlines();

      // test ok
      // (3,4,)
      if p.at(T![')']) {
        break;
      }
    } else {
      break;
    }
  }

  p.expect(T![')']);
}

fn call_block_expr(p: &mut Parser) {
  let m = p.start();
  // test ok
  // hi {
  //   3
  //   4
  // }
  super::item::block_items(p);
  m.complete(p, BLOCK_ARGUMENTS);
}

fn postfix_dot_expr(
  p: &mut Parser,
  lhs: CompletedMarker,
) -> Result<CompletedMarker, CompletedMarker> {
  match p.peek() {
    T![ident] => {
      let m = lhs.precede(p);
      p.eat(T![.]);
      p.eat(T![ident]);
      Ok(m.complete(p, FIELD_EXPR))
    }
    _ => Err(lhs),
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

    T![true] => {
      p.eat(T![true]);
      Some(m.complete(p, LIT_EXPR))
    }
    T![false] => {
      p.eat(T![false]);
      Some(m.complete(p, LIT_EXPR))
    }

    SINGLE_QUOTE => {
      p.eat(SINGLE_QUOTE);

      character_lit(p);

      Some(m.complete(p, CHARACTER_LIT))
    }

    DOUBLE_QUOTE => {
      p.eat(DOUBLE_QUOTE);

      double_quote_string(p);

      Some(m.complete(p, DOUBLE_QUOTED_STRING))
    }

    TRIPPLE_QUOTE => {
      p.eat(TRIPPLE_QUOTE);

      tripple_quote_string(p);

      Some(m.complete(p, TRIPPLE_QUOTED_STRING))
    }

    // test ok
    // s"hello $world"
    IDENT if p.peek() == DOUBLE_QUOTE => {
      p.eat(IDENT);
      p.eat(DOUBLE_QUOTE);

      // FIXME: Need to parse interpolated strings.
      double_quote_string(p);

      Some(m.complete(p, DOUBLE_QUOTED_STRING))
    }

    // test ok
    // s"""hello $world"""
    IDENT if p.peek() == TRIPPLE_QUOTE => {
      p.eat(IDENT);
      p.eat(TRIPPLE_QUOTE);

      // FIXME: Need to parse interpolated strings.
      tripple_quote_string(p);

      Some(m.complete(p, TRIPPLE_QUOTED_STRING))
    }

    IDENT => {
      p.eat(IDENT);
      Some(m.complete(p, IDENT_EXPR))
    }

    T![return] => {
      // test ok
      // return 3
      p.eat(T![return]);
      // test ok
      // return
      if !p.at(T![nl]) {
        expr(p);
      }
      Some(m.complete(p, RETURN_EXPR))
    }

    T![new] => {
      p.eat(T![new]);

      // test ok
      // new Iterator
      if p.at(T![ident]) {
        p.eat(T![ident]);
      }

      // test ok
      // new Iterator[Int]
      // new Iterator[String]("hello")
      if p.at(T!['[']) {
        super::type_expr::type_params(p);
      }

      // test ok
      // new Iterator()
      // new Iterator("hello")
      if p.at(T!['(']) {
        let m = p.start();
        call_paren_expr(p);
        m.complete(p, PAREN_ARGUMENTS);
      }

      // test ok
      // new { def next = None }
      // new Iterator { def next = None }
      if p.at(T!['{']) {
        call_block_expr(p);
      }

      Some(m.complete(p, NEW_CLASS_EXPR))
    }

    T!['{'] => {
      // test ok
      // {
      //   2 + 3
      //   5 + 6
      // }
      super::item::block_items(p);
      Some(m.complete(p, BLOCK_EXPR))
    }

    T!['('] => {
      // test ok
      // ()
      // (1 to 100)
      // (1, 2)
      tuple_expr(p);
      Some(m.complete(p, TUPLE_EXPR))
    }

    _ => {
      m.abandon(p);
      p.error(format!("expected expression, got {:?}", p.current()));
      p.recover_until(T![nl]);
      None
    }
  }
}

pub fn character_lit(p: &mut Parser) {
  // test ok
  // 'a'
  // '*'
  match p.current() {
    // test err
    // ''
    SINGLE_QUOTE => {
      p.error("unexpected empty character literal");
      p.eat(SINGLE_QUOTE);
      return;
    }
    // test err
    // 'a
    EOF => {
      p.error("unexpected end of file");
      return;
    }

    // test ok
    // '3'
    _ => {
      // test err
      // 'ab'
      if p.slice().chars().count() > 1 {
        p.error("expected a single character");
      }
      p.bump();
    }
  }

  match p.current() {
    SINGLE_QUOTE => {
      p.eat(SINGLE_QUOTE);
      return;
    }

    _ => {
      p.error("expected a single character");
      p.bump();
      return;
    }
  }
}

pub fn double_quote_string(p: &mut Parser) {
  // test ok
  // "hello"
  loop {
    match p.current() {
      DOUBLE_QUOTE => {
        p.eat(DOUBLE_QUOTE);
        break;
      }
      // test err
      // "hello
      EOF => {
        p.error("unexpected end of file");
        break;
      }
      // test ok
      // "hello\"world"
      IDENT if p.slice() == "\\" => {
        p.bump();

        // TODO: Parse unicode escapes and such.
        p.bump();
      }
      _ => {
        p.bump();
      }
    }
  }
}

pub fn tripple_quote_string(p: &mut Parser) {
  // test ok
  // """hello"""
  loop {
    match p.current() {
      TRIPPLE_QUOTE => {
        p.eat(TRIPPLE_QUOTE);
        break;
      }
      // test err
      // """hello
      EOF => {
        p.error("unexpected end of file");
        break;
      }
      // test ok
      // """hello
      // world"""
      _ => {
        p.bump();
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::tests::{check, check_expr};

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
      "true + false",
      expect![@r#"
        INFIX_EXPR
          LIT_EXPR
            TRUE_KW 'true'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            FALSE_KW 'false'
      "#],
    );

    check_expr(
      "\"hi\"",
      expect![@r#"
        DOUBLE_QUOTED_STRING
          DOUBLE_QUOTE '"'
          IDENT 'hi'
          DOUBLE_QUOTE '"'
      "#],
    );

    check_expr(
      "\"foo \\\" bar {{\"",
      expect![@r#"
        DOUBLE_QUOTED_STRING
          DOUBLE_QUOTE '"'
          IDENT 'foo'
          WHITESPACE ' '
          IDENT '\'
          DOUBLE_QUOTE '"'
          WHITESPACE ' '
          IDENT 'bar'
          WHITESPACE ' '
          OPEN_CURLY '{'
          OPEN_CURLY '{'
          DOUBLE_QUOTE '"'
      "#],
    );

    check_expr(
      "'a'",
      expect![@r#"
        CHARACTER_LIT
          SINGLE_QUOTE '''
          IDENT 'a'
          SINGLE_QUOTE '''
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

    check_expr(
      "2 +\n 3",
      expect![@r#"
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '+'
          NL_KW '\n'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
      "#],
    );

    check_expr(
      "2 +\n\n 3",
      expect![@r#"
        LIT_EXPR
          INT_LIT_KW '2'
        WHITESPACE ' '
        IDENT '+'
        NL_KW '\n'
        NL_KW '\n'
        error: expected expression, got expression separator INT_LIT_KW
        WHITESPACE ' '
        INT_LIT_KW '3'
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
          DOUBLE_QUOTED_STRING
            DOUBLE_QUOTE '"'
            IDENT 'hi'
            DOUBLE_QUOTE '"'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          DOUBLE_QUOTED_STRING
            DOUBLE_QUOTE '"'
            IDENT 'there'
            DOUBLE_QUOTE '"'
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
  fn lambda_expr() {
    // This might be a bit difficult to parse up the chain. Ah well, this way avoids
    // special cases in the parser, which is nice.
    check_expr(
      "x => x + 3",
      expect![@r#"
        INFIX_EXPR
          IDENT_EXPR
            IDENT 'x'
          WHITESPACE ' '
          FAT_ARROW '=>'
          WHITESPACE ' '
          INFIX_EXPR
            IDENT_EXPR
              IDENT 'x'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );

    // FIXME: Type annotations are valid anywhere, so we should be able to parse
    // `x: Int` as an expression.
    check_expr(
      "(x: Int) => x + 3",
      expect![@r#"
        INFIX_EXPR
          TUPLE_EXPR
            OPEN_PAREN '('
            IDENT_EXPR
              IDENT 'x'
            error: expected expression, got COLON
            COLON ':'
            WHITESPACE ' '
            IDENT 'Int'
            CLOSE_PAREN ')'
          WHITESPACE ' '
          FAT_ARROW '=>'
          WHITESPACE ' '
          INFIX_EXPR
            IDENT_EXPR
              IDENT 'x'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );
  }

  #[test]
  fn call_op() {
    check_expr(
      "hi(2)",
      expect![@r#"
        CALL_EXPR
          IDENT_EXPR
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
          IDENT_EXPR
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

    check_expr(
      "hi { 3 }",
      expect![@r#"
        CALL_EXPR
          IDENT_EXPR
            IDENT 'hi'
          WHITESPACE ' '
          BLOCK_ARGUMENTS
            OPEN_CURLY '{'
            WHITESPACE ' '
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '3'
            WHITESPACE ' '
            CLOSE_CURLY '}'
      "#],
    );

    check_expr(
      "hi(foo = 3, bar = 4)",
      expect![@r#"
        CALL_EXPR
          IDENT_EXPR
            IDENT 'hi'
          PAREN_ARGUMENTS
            OPEN_PAREN '('
            IDENT 'foo'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
            COMMA ','
            WHITESPACE ' '
            IDENT 'bar'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '4'
            CLOSE_PAREN ')'
      "#],
    );
  }

  #[test]
  fn dot_exprs() {
    check_expr(
      "foo.bar",
      expect![@r#"
        FIELD_EXPR
          IDENT_EXPR
            IDENT 'foo'
          DOT '.'
          IDENT 'bar'
      "#],
    );

    check_expr(
      "foo.bar.baz",
      expect![@r#"
        FIELD_EXPR
          FIELD_EXPR
            IDENT_EXPR
              IDENT 'foo'
            DOT '.'
            IDENT 'bar'
          DOT '.'
          IDENT 'baz'
      "#],
    );

    check_expr(
      "foo.3",
      expect![@r#"
        IDENT_EXPR
          IDENT 'foo'
        DOT '.'
        error: expected expression, got DOT
        INT_LIT_KW '3'
      "#],
    );
  }

  #[test]
  fn block_expr() {
    check_expr(
      "{ 2 + 3 }",
      expect![@r#"
        BLOCK_EXPR
          OPEN_CURLY '{'
          WHITESPACE ' '
          EXPR_ITEM
            INFIX_EXPR
              LIT_EXPR
                INT_LIT_KW '2'
              WHITESPACE ' '
              IDENT '+'
              WHITESPACE ' '
              LIT_EXPR
                INT_LIT_KW '3'
          WHITESPACE ' '
          CLOSE_CURLY '}'
      "#],
    );
  }

  #[test]
  fn newline_magic() {
    check(
      "println {
         3
       }",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            CALL_EXPR
              IDENT_EXPR
                IDENT 'println'
              WHITESPACE ' '
              BLOCK_ARGUMENTS
                OPEN_CURLY '{'
                NL_KW '\n'
                WHITESPACE '         '
                EXPR_ITEM
                  LIT_EXPR
                    INT_LIT_KW '3'
                  NL_KW '\n'
                WHITESPACE '       '
                CLOSE_CURLY '}'
      "#],
    );

    // TODO: This should parse the same as the above
    check(
      "println
       {
         3
       }",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            IDENT_EXPR
              IDENT 'println'
            NL_KW '\n'
          WHITESPACE '       '
          EXPR_ITEM
            BLOCK_EXPR
              OPEN_CURLY '{'
              NL_KW '\n'
              WHITESPACE '         '
              EXPR_ITEM
                LIT_EXPR
                  INT_LIT_KW '3'
                NL_KW '\n'
              WHITESPACE '       '
              CLOSE_CURLY '}'
      "#],
    );

    // NOTE: This should parse as two expressions lol
    check(
      "println

       {
         3
       }",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            IDENT_EXPR
              IDENT 'println'
            NL_KW '\n'
          NL_KW '\n'
          WHITESPACE '       '
          EXPR_ITEM
            BLOCK_EXPR
              OPEN_CURLY '{'
              NL_KW '\n'
              WHITESPACE '         '
              EXPR_ITEM
                LIT_EXPR
                  INT_LIT_KW '3'
                NL_KW '\n'
              WHITESPACE '       '
              CLOSE_CURLY '}'
      "#],
    );
  }

  #[test]
  fn new_expr() {
    check(
      "new Iterator",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            NEW_CLASS_EXPR
              NEW_KW 'new'
              WHITESPACE ' '
              IDENT 'Iterator'
      "#],
    );

    check(
      "new Iterator()",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            NEW_CLASS_EXPR
              NEW_KW 'new'
              WHITESPACE ' '
              IDENT 'Iterator'
              PAREN_ARGUMENTS
                OPEN_PAREN '('
                CLOSE_PAREN ')'
      "#],
    );

    check(
      "new Iterator(2, 3)",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            NEW_CLASS_EXPR
              NEW_KW 'new'
              WHITESPACE ' '
              IDENT 'Iterator'
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

    check(
      "new Iterator { def next = None }",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            NEW_CLASS_EXPR
              NEW_KW 'new'
              WHITESPACE ' '
              IDENT 'Iterator'
              WHITESPACE ' '
              BLOCK_ARGUMENTS
                OPEN_CURLY '{'
                WHITESPACE ' '
                FUN_DEF
                  DEF_KW 'def'
                  WHITESPACE ' '
                  FUN_SIG
                    IDENT 'next'
                  WHITESPACE ' '
                  EQ '='
                  WHITESPACE ' '
                  IDENT_EXPR
                    IDENT 'None'
                WHITESPACE ' '
                CLOSE_CURLY '}'
      "#],
    );
  }

  #[test]
  fn match_exprs() {
    check(
      "2 match {
        case 1 => 3
        case 2 => 4
        case _ => 5
      }",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            MATCH_EXPR
              LIT_EXPR
                INT_LIT_KW '2'
              WHITESPACE ' '
              MATCH_KW 'match'
              WHITESPACE ' '
              OPEN_CURLY '{'
              NL_KW '\n'
              WHITESPACE '        '
              CASE_ITEM
                CASE_KW 'case'
                WHITESPACE ' '
                LIT_PATTERN
                  INT_LIT_KW '1'
                WHITESPACE ' '
                FAT_ARROW '=>'
                WHITESPACE ' '
                BLOCK
                  EXPR_ITEM
                    LIT_EXPR
                      INT_LIT_KW '3'
                    NL_KW '\n'
              WHITESPACE '        '
              CASE_ITEM
                CASE_KW 'case'
                WHITESPACE ' '
                LIT_PATTERN
                  INT_LIT_KW '2'
                WHITESPACE ' '
                FAT_ARROW '=>'
                WHITESPACE ' '
                BLOCK
                  EXPR_ITEM
                    LIT_EXPR
                      INT_LIT_KW '4'
                    NL_KW '\n'
              WHITESPACE '        '
              CASE_ITEM
                CASE_KW 'case'
                WHITESPACE ' '
                IDENT_PATTERN
                  IDENT '_'
                WHITESPACE ' '
                FAT_ARROW '=>'
                WHITESPACE ' '
                BLOCK
                  EXPR_ITEM
                    LIT_EXPR
                      INT_LIT_KW '5'
                    NL_KW '\n'
              WHITESPACE '      '
              CLOSE_CURLY '}'
      "#],
    );

    check(
      "2 match { case 1 => 3 }",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            MATCH_EXPR
              LIT_EXPR
                INT_LIT_KW '2'
              WHITESPACE ' '
              MATCH_KW 'match'
              WHITESPACE ' '
              OPEN_CURLY '{'
              WHITESPACE ' '
              CASE_ITEM
                CASE_KW 'case'
                WHITESPACE ' '
                LIT_PATTERN
                  INT_LIT_KW '1'
                WHITESPACE ' '
                FAT_ARROW '=>'
                WHITESPACE ' '
                BLOCK
                  EXPR_ITEM
                    LIT_EXPR
                      INT_LIT_KW '3'
              WHITESPACE ' '
              CLOSE_CURLY '}'
      "#],
    );

    check(
      "2 match { case 0 => 1\n case 2 => 3 }",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            MATCH_EXPR
              LIT_EXPR
                INT_LIT_KW '2'
              WHITESPACE ' '
              MATCH_KW 'match'
              WHITESPACE ' '
              OPEN_CURLY '{'
              WHITESPACE ' '
              CASE_ITEM
                CASE_KW 'case'
                WHITESPACE ' '
                LIT_PATTERN
                  INT_LIT_KW '0'
                WHITESPACE ' '
                FAT_ARROW '=>'
                WHITESPACE ' '
                BLOCK
                  EXPR_ITEM
                    LIT_EXPR
                      INT_LIT_KW '1'
                    NL_KW '\n'
              WHITESPACE ' '
              CASE_ITEM
                CASE_KW 'case'
                WHITESPACE ' '
                LIT_PATTERN
                  INT_LIT_KW '2'
                WHITESPACE ' '
                FAT_ARROW '=>'
                WHITESPACE ' '
                BLOCK
                  EXPR_ITEM
                    LIT_EXPR
                      INT_LIT_KW '3'
              WHITESPACE ' '
              CLOSE_CURLY '}'
      "#],
    );
  }

  #[test]
  fn tuple_exprs() {
    check(
      "()",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            TUPLE_EXPR
              OPEN_PAREN '('
              CLOSE_PAREN ')'
      "#],
    );

    check(
      "(2, 3)",
      expect![@r#"
        SOURCE_FILE
          EXPR_ITEM
            TUPLE_EXPR
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
