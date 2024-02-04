use crate::Marker;

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
  let m = p.start();

  // test ok
  // private def foo = 3
  // protected val bar = 3
  match p.current() {
    T![private] | T![protected] => {
      p.bump();
      p.eat_newlines();
    }
    _ => {}
  }

  match p.current() {
    T![import] => import_item(p, m),
    T![def] => fun_def(p, m),
    T![val] => val_def(p, m),
    T![case] | T![class] => class_def(p, m),

    _ => {
      expr::expr(p);
      m.complete(p, EXPR_ITEM);
    }
  };
}

// test ok
// import foo.bar.baz
// import xxx.yyy.zzz
fn import_item(p: &mut Parser, m: Marker) {
  p.eat(T![import]);

  let path = p.start();

  loop {
    match p.current() {
      T![ident] => {
        p.bump();
      }

      T![.] => {
        p.bump();
        continue;
      }

      T!['{'] => {
        let path = path.complete(p, PATH);
        let selector = path.precede(p);
        import_list(p, selector);
        m.complete(p, IMPORT);
        return;
      }

      T![nl] | EOF => {
        path.complete(p, PATH);
        m.complete(p, IMPORT);
        return;
      }

      // test err
      // import 3
      _ => {
        path.abandon(p);
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
fn import_list(p: &mut Parser, m: Marker) {
  p.eat(T!['{']);
  loop {
    match p.current() {
      T![ident] => {
        let m = p.start();
        p.eat(T![ident]);
        m.complete(p, IMPORT_SELECTOR_ID);
      }

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
fn class_def(p: &mut Parser, m: Marker) {
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
fn fun_def(p: &mut Parser, m: Marker) {
  p.eat(T![def]);
  fun_sig(p);

  p.expect(T![=]);
  expr::expr(p);

  m.complete(p, FUN_DEF);
}

// test ok
// def foo(a: String, b: String): List[Int] = 2
fn fun_sig(p: &mut Parser) {
  let m = p.start();
  p.expect(T![ident]);

  // test ok
  // def foo(a: String) = 2
  if p.at(T!['(']) {
    fun_params(p);
  }

  // test ok
  // def bar: Int = 2
  if p.at(T![:]) {
    p.eat(T![:]);
    super::type_expr::type_expr(p);
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

  super::type_expr::type_expr(p);

  m.complete(p, FUN_PARAM);
}

// test ok
// val foo = 3
fn val_def(p: &mut Parser, m: Marker) {
  p.eat(T![val]);
  p.expect(T![ident]);

  let mut found_type = false;
  let mut found_expr = false;

  // test ok
  // val foo: Int
  // val bar: String
  if p.at(T![:]) {
    p.eat(T![:]);

    found_type = true;
    super::type_expr::type_expr(p);
  }

  if p.at(T![=]) {
    p.eat(T![=]);

    // test ok
    // val foo =
    //   3
    p.eat_newlines();

    found_expr = true;
    expr::expr(p);
  }

  if !found_type && !found_expr {
    p.error("expected type or expr");
  }

  m.complete(p, VAL_DEF);
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
                  SIMPLE_TYPE
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
                  SIMPLE_TYPE
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
                  SIMPLE_TYPE
                    IDENT 'Int'
                COMMA ','
                WHITESPACE ' '
                FUN_PARAM
                  IDENT 'b'
                  COLON ':'
                  WHITESPACE ' '
                  SIMPLE_TYPE
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

  #[test]
  fn val_def() {
    check(
      "val foo = 3",
      expect![@r#"
        SOURCE_FILE
          VAL_DEF
            VAL_KW 'val'
            WHITESPACE ' '
            IDENT 'foo'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );

    check(
      "val foo: Int = 3",
      expect![@r#"
        SOURCE_FILE
          VAL_DEF
            VAL_KW 'val'
            WHITESPACE ' '
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            SIMPLE_TYPE
              IDENT 'Int'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
      "#],
    );
  }
}
