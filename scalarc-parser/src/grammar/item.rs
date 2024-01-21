use super::*;

pub fn mod_items(p: &mut Parser) {
  while !p.at(EOF) {
    item(p);
  }
}

fn item(p: &mut Parser) {
  let m = p.start();

  match p.current() {
    T![import] => import_item(p, m),
    T![def] => fun_dec(p, m),

    /*
    T![type] => type_alias(p, m),
    T![struct] => adt::strukt(p, m),
    T![enum] => adt::enum_(p, m),
    IDENT if p.at_contextual_kw(T![union]) && p.nth(1) == IDENT => adt::union(p, m),

    T![macro] => macro_def(p, m),
    IDENT if p.at_contextual_kw(T![macro_rules]) && p.nth(1) == BANG => macro_rules(p, m),

    T![const] if (la == IDENT || la == T![_] || la == T![mut]) => consts::konst(p, m),
    T![static] if (la == IDENT || la == T![_] || la == T![mut]) => consts::static_(p, m),
    */
    _ => {
      p.error_bump(format!("expected item, got {:?}", p.current()));
      m.abandon(p);
    }
  };
}

// test ok
// import foo.bar.baz
fn import_item(p: &mut Parser, m: Marker) {
  p.eat(T![import]);
  loop {
    match p.current() {
      T![ident] => {
        p.bump();
      }

      T![.] => {
        p.bump();
        continue;
      }

      T!['{'] => import_list(p),

      T![nl] => {
        p.eat(T![nl]);
        m.complete(p, IMPORT);
        return;
      }

      // test err
      // import 3
      _ => {
        p.error(format!("expected ident, got {:?}", p.current()));
        p.recover_until(T![nl]);
        p.eat(T![nl]);
        m.abandon(p);
        return;
      }
    }
  }
}

// test ok
// import foo.{ bar, baz }
fn import_list(p: &mut Parser) {
  let m = p.start();
  p.eat(T!['{']);
  loop {
    match p.current() {
      T![ident] => p.eat(T![ident]),
      T![,] => p.eat(T![,]),

      T!['}'] => {
        p.eat(T!['}']);
        break;
      }

      _ => break,
    }
  }

  m.complete(p, IMPORT_SELECTORS);
}

// test ok
// def foo = 3
fn fun_dec(p: &mut Parser, m: Marker) {
  p.eat(T![def]);
  fun_sig(p);

  p.expect(T![=]);
  expr::expr(p);

  p.expect(T![nl]);
  m.complete(p, FUN_DEC);
}

fn fun_sig(p: &mut Parser) {
  let m = p.start();
  p.expect(T![ident]);

  if p.current() == T!['('] {
    fun_params(p);
  }
  m.complete(p, FUN_SIG);
}

fn fun_params(p: &mut Parser) {
  let m = p.start();
  p.eat(T!['(']);

  // test ok
  // def foo() = 3
  if p.current() == T![')'] {
    p.eat(T![')']);
    m.complete(p, FUN_PARAMS);
    return;
  }

  loop {
    fun_param(p);
    // test ok
    // def foo(a: Int, b: String) = 3
    if p.current() == T![,] {
      p.eat(T![,]);
    } else {
      p.expect(T![')']);
      m.complete(p, FUN_PARAMS);
      break;
    }
  }
}

// test ok
// def foo(a: Int) = 3
fn fun_param(p: &mut Parser) {
  let m = p.start();
  p.expect(T![ident]);
  if p.current() == T![:] {
    p.eat(T![:]);
    p.expect(T![ident]);
  }
  m.complete(p, FUN_PARAM);
}

#[cfg(test)]
mod tests {
  use crate::tests::check;

  #[test]
  fn imports() {
    check(
      "import foo.bar.baz",
      expect![@r#"
        SOURCE_FILE
          IMPORT
            IMPORT_KW 'import'
            WHITESPACE ' '
            IDENT 'foo'
            DOT '.'
            IDENT 'bar'
            DOT '.'
            IDENT 'baz'
            NL_KW '\n'
      "#],
    );

    check(
      "import foo.{ bar, baz }",
      expect![@r#"
        SOURCE_FILE
          IMPORT
            IMPORT_KW 'import'
            WHITESPACE ' '
            IDENT 'foo'
            DOT '.'
            IMPORT_SELECTORS
              OPEN_CURLY '{'
              WHITESPACE ' '
              IDENT 'bar'
              COMMA ','
              WHITESPACE ' '
              IDENT 'baz'
              WHITESPACE ' '
              CLOSE_CURLY '}'
            NL_KW '\n'
      "#],
    );
  }

  #[test]
  fn fun_dec() {
    check(
      "def foo = 3",
      expect![@r#"
        SOURCE_FILE
          FUN_DEC
            DEF_KW 'def'
            WHITESPACE ' '
            IDENT 'foo'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LITERAL
              INT_LIT_KW '3'
            NL_KW '\n'
      "#],
    );

    check(
      "def foo(a:) = 3",
      expect![@r#"
        SOURCE_FILE
          FUN_DEC
            DEF_KW 'def'
            WHITESPACE ' '
            IDENT 'foo'
            FUN_PARAMS
              OPEN_PAREN '('
              FUN_PARAM
                IDENT 'a'
                COLON ':'
                error: expected IDENT
              CLOSE_PAREN ')'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LITERAL
              INT_LIT_KW '3'
            NL_KW '\n'
      "#],
    );

    check(
      "def foo(a: Int) = 3",
      expect![@r#"
        SOURCE_FILE
          FUN_DEC
            DEF_KW 'def'
            WHITESPACE ' '
            IDENT 'foo'
            FUN_PARAMS
              OPEN_PAREN '('
              FUN_PARAM
                IDENT 'a'
                COLON ':'
                WHITESPACE ' '
                IDENT 'Int'
              CLOSE_PAREN ')'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LITERAL
              INT_LIT_KW '3'
            NL_KW '\n'
      "#],
    );

    check(
      "def foo(a: Int, b: String) = 3",
      expect![@r#"
        SOURCE_FILE
          FUN_DEC
            DEF_KW 'def'
            WHITESPACE ' '
            IDENT 'foo'
            FUN_PARAMS
              OPEN_PAREN '('
              FUN_PARAM
                IDENT 'a'
                COLON ':'
                WHITESPACE ' '
                IDENT 'Int'
              COMMA ','
              WHITESPACE ' '
              FUN_PARAM
                IDENT 'b'
                COLON ':'
                WHITESPACE ' '
                IDENT 'String'
              CLOSE_PAREN ')'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LITERAL
              INT_LIT_KW '3'
            NL_KW '\n'
      "#],
    );
  }
}
