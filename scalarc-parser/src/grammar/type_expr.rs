use crate::{
  syntax_kind::{SyntaxKind::*, T},
  Parser,
};

pub fn type_expr(p: &mut Parser) {
  let m = p.start();
  p.expect(T![ident]);
  let mut lhs = m.complete(p, SIMPLE_TYPE);

  loop {
    p.eat_newlines();

    match p.current() {
      T![.] => {
        let m = lhs.precede(p);
        p.eat(T![.]);
        p.expect(T![ident]);
        lhs = m.complete(p, PATH_TYPE);
      }
      T!['['] => {
        let m = lhs.precede(p);
        {
          let m = p.start();
          p.eat(T!['[']);
          p.eat_newlines();
          type_expr(p);
          p.eat_newlines();
          p.expect(T![']']);
          m.complete(p, TYPE_ARGS);
        }
        lhs = m.complete(p, GENERIC_TYPE);
      }

      T![,] | T![']'] | T![')'] | T!['}'] | T![=] => {
        return;
      }

      _ => {
        p.error(format!("expected type, got {:?}", p.current()));
        p.recover_until_any(&[T![nl], T![,], T![')'], T!['}'], T![=]]);
        return;
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::tests::check;

  #[test]
  fn type_paths() {
    check(
      "val foo: Foo.Bar = 3",
      expect![@r#"
        SOURCE_FILE
          VAL_DEF
            VAL_KW 'val'
            WHITESPACE ' '
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            PATH_TYPE
              SIMPLE_TYPE
                IDENT 'Foo'
              DOT '.'
              IDENT 'Bar'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );

    check(
      "val foo: Foo.Bar.Baz = 3",
      expect![@r#"
        SOURCE_FILE
          VAL_DEF
            VAL_KW 'val'
            WHITESPACE ' '
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            PATH_TYPE
              PATH_TYPE
                SIMPLE_TYPE
                  IDENT 'Foo'
                DOT '.'
                IDENT 'Bar'
              DOT '.'
              IDENT 'Baz'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );
  }

  #[test]
  fn type_params() {
    check(
      "val foo: Foo[Int] = 3",
      expect![@r#"
        SOURCE_FILE
          VAL_DEF
            VAL_KW 'val'
            WHITESPACE ' '
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            GENERIC_TYPE
              SIMPLE_TYPE
                IDENT 'Foo'
              TYPE_ARGS
                OPEN_BRACKET '['
                SIMPLE_TYPE
                  IDENT 'Int'
                CLOSE_BRACKET ']'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );
  }
}
