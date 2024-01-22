use super::*;

pub fn mod_items(p: &mut Parser) { items(p, false); }
pub fn block_items(p: &mut Parser) {
  p.eat(T!['{']);
  items(p, true);
}

// test ok
// class Foo {
//   def bar = 3
// }
fn items(p: &mut Parser, end_in_brace: bool) {
  let mut is_first = true;
  'items: loop {
    // Eat trailing newlines, and update `found_newline` to make sure there is a
    // newline between each item.
    let mut found_newline = false;
    loop {
      if p.at(T![nl]) {
        found_newline = true;
        p.eat(T![nl]);
      } else if p.at(EOF) {
        if end_in_brace {
          p.error("missing closing '}'");
        }
        break 'items;
      } else if p.at(T!['}']) {
        if !end_in_brace {
          p.error_bump("unexpected '}'");
        } else {
          p.bump();
          break 'items;
        }
      } else {
        break;
      }
    }

    if !found_newline && !is_first {
      p.error("expected newline");
    }
    is_first = false;

    item(p);
  }
}

fn item(p: &mut Parser) {
  match p.current() {
    T![import] => import_item(p),
    T![def] => fun_def(p),
    T![case] | T![class] => class_def(p),

    _ => {
      let m = p.start();
      expr::expr(p);
      m.complete(p, EXPR_ITEM);
    }
  };
}

// test ok
// import foo.bar.baz
// import xxx.yyy.zzz
fn import_item(p: &mut Parser) {
  let m = p.start();

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

      T![nl] | EOF => {
        m.complete(p, IMPORT);
        return;
      }

      // test err
      // import 3
      _ => {
        p.error(format!("expected ident, got {:?}", p.current()));
        p.recover_until(T![nl]);
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
// class Foo() {}
fn class_def(p: &mut Parser) {
  let m = p.start();
  // test ok
  // case class Foo() {}
  if p.current() == T![case] {
    p.eat(T![case]);
    p.expect(T![class]);
  } else {
    p.eat(T![class]);
  }

  p.expect(T![ident]);

  // test ok
  // class Foo {}
  if p.current() == T!['('] {
    fun_params(p);
  }

  // test ok
  // class Foo
  if p.current() == T!['{'] {
    item_body(p);
  }

  m.complete(p, CLASS_DEF);
}

fn item_body(p: &mut Parser) {
  let m = p.start();
  p.eat(T!['{']);

  items(p, true);

  m.complete(p, ITEM_BODY);
}

// test ok
// def foo = 3
fn fun_def(p: &mut Parser) {
  let m = p.start();

  p.eat(T![def]);
  fun_sig(p);

  p.expect(T![=]);
  expr::expr(p);

  m.complete(p, FUN_DEF);
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
  if p.at(T![')']) {
    p.eat(T![')']);
    m.complete(p, FUN_PARAMS);
    return;
  }

  // test ok
  // def foo(
  //   a: Int
  // ) = 3
  p.eat_newlines();

  loop {
    fun_param(p);
    p.eat_newlines();
    // test ok
    // def foo(a: Int, b: String) = 3
    if p.current() == T![,] {
      p.eat(T![,]);

      // test ok
      // def foo(
      //   a: Int
      //   ,
      //   b: Int
      // ) = 3
      p.eat_newlines();
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

  p.expect(T![:]);
  // test ok
  // def foo(
  //   a:
  //   Int
  // ) = 3
  p.eat_newlines();

  // TODO: Parse types
  p.expect(T![ident]);

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
      "#],
    );
  }

  #[test]
  fn fun_dec() {
    check(
      "def foo = 3",
      expect![@r#"
        SOURCE_FILE
          FUN_DEF
            DEF_KW 'def'
            WHITESPACE ' '
            FUN_SIG
              IDENT 'foo'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );

    check(
      "def foo(a:) = 3",
      expect![@r#"
        SOURCE_FILE
          FUN_DEF
            DEF_KW 'def'
            WHITESPACE ' '
            FUN_SIG
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
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );

    check(
      "def foo(a: Int) = 3",
      expect![@r#"
        SOURCE_FILE
          FUN_DEF
            DEF_KW 'def'
            WHITESPACE ' '
            FUN_SIG
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
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );

    check(
      "def foo(a: Int, b: String) = 3",
      expect![@r#"
        SOURCE_FILE
          FUN_DEF
            DEF_KW 'def'
            WHITESPACE ' '
            FUN_SIG
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
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );
  }

  #[test]
  fn newline_handling() {
    check(
      "class Foo() { class Bar() {} class Baz() {} }",
      expect![@r#"
        SOURCE_FILE
          CLASS_DEF
            CLASS_KW 'class'
            WHITESPACE ' '
            IDENT 'Foo'
            FUN_PARAMS
              OPEN_PAREN '('
              CLOSE_PAREN ')'
            WHITESPACE ' '
            ITEM_BODY
              OPEN_CURLY '{'
              WHITESPACE ' '
              CLASS_DEF
                CLASS_KW 'class'
                WHITESPACE ' '
                IDENT 'Bar'
                FUN_PARAMS
                  OPEN_PAREN '('
                  CLOSE_PAREN ')'
                WHITESPACE ' '
                ITEM_BODY
                  OPEN_CURLY '{'
                  CLOSE_CURLY '}'
              error: expected newline
              WHITESPACE ' '
              CLASS_DEF
                CLASS_KW 'class'
                WHITESPACE ' '
                IDENT 'Baz'
                FUN_PARAMS
                  OPEN_PAREN '('
                  CLOSE_PAREN ')'
                WHITESPACE ' '
                ITEM_BODY
                  OPEN_CURLY '{'
                  CLOSE_CURLY '}'
              WHITESPACE ' '
              CLOSE_CURLY '}'
      "#],
    );
  }
}
