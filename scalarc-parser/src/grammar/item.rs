use crate::Marker;

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockTerminator {
  Nothing,
  Brace,
  Case,
  Lambda,
}

pub fn mod_items(p: &mut Parser) { items(p, BlockTerminator::Nothing); }
pub fn block_items(p: &mut Parser) {
  p.eat(T!['{']);
  items(p, BlockTerminator::Brace);
}

pub fn lambda_items(p: &mut Parser) { items(p, BlockTerminator::Lambda); }

// test ok
// class Foo {
//   def foo
//   def bar = 3
// }
fn items(p: &mut Parser, terminator: BlockTerminator) {
  let mut is_first = true;
  'items: loop {
    // Eat trailing newlines, and update `found_newline` to make sure there is a
    // newline between each item.
    let mut found_newline = false;
    loop {
      if p.at(T![nl]) {
        found_newline = true;

        // Special case: If we have a `nl` then a `case`, break early, so that the `nl`
        // gets picked up by the outer block.
        //
        // test ok
        // {
        //   case 3 => 3
        //   case 4 => 4
        // }
        if p.peek() == T![case] && terminator == BlockTerminator::Case {
          break 'items;
        }

        p.eat(T![nl]);
      } else if p.at(EOF) {
        if terminator == BlockTerminator::Brace {
          p.error("missing closing '}'");
        }
        break 'items;
      } else if p.at(T!['}']) {
        match terminator {
          BlockTerminator::Brace => {
            p.eat(T!['}']);
            break 'items;
          }
          BlockTerminator::Nothing => {
            p.error_bump("unexpected '}'");
          }
          BlockTerminator::Case | BlockTerminator::Lambda => {
            break 'items;
          }
        }
      } else if terminator == BlockTerminator::Lambda {
        // test ok
        // println(() => 2, () => 3)
        match p.current() {
          T![nl] | T![=>] | T![,] | T![')'] | T!['}'] | T![else] | EOF => break 'items,
          _ => break,
        }
      } else {
        break;
      }
    }

    if p.at(T![case]) && terminator == BlockTerminator::Case {
      if is_first {
        p.error("expected case expression");
      } else if !found_newline {
        p.error("expected newline");
      }
      break 'items;
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

  if p.at(T![@]) {
    let m = p.start();
    annotation(p);
    m.complete(p, ANNOTATION);
  }

  // Scala doesn't define an order to these, so we just parse all of them.
  loop {
    // test ok
    // private def foo = 3
    // protected val bar = 3
    // final def foo = 3
    // private final def foo = 4
    // implicit private def foo = 5
    // private implicit def foo = 5
    // final abstract class Int extends AnyVal {}
    // override def foo() = {}
    // sealed trait Foo
    // lazy val bar = 3
    match p.current() {
      T![private]
      | T![protected]
      | T![final]
      | T![implicit]
      | T![sealed]
      | T![abstract]
      | T![override]
      | T![lazy] => {
        let m = p.start();

        match p.current() {
          T![private] | T![protected] => {
            p.bump();
            access_qualifier(p)
          }
          _ => {
            p.bump();
          }
        }

        p.eat_newlines();
        m.complete(p, MODIFIER);
      }
      _ => break,
    }
  }

  match p.current() {
    T![package] => package_item(p, m),

    T![import] => import_item(p, m),
    T![def] => fun_def(p, m),
    T![val] | T![var] => val_def(p, m),

    T![class] | T![trait] | T![object] => class_def(p, m),
    T![case] if matches!(p.peek(), T![class] | T![object]) => class_def(p, m),

    T![case] => case_item(p, m),

    _ => {
      expr::expr(p);
      m.complete(p, EXPR_ITEM);
    }
  };
}

// test ok
// private[this] def foo = 3
fn access_qualifier(p: &mut Parser) {
  if p.at(T!['[']) {
    let m = p.start();
    p.eat(T!['[']);
    // FIXME: Parse a path.
    p.expect(T![ident]);
    p.expect(T![']']);
    m.complete(p, ACCESS_QUALIFIER);
  }
}

// test ok
// @volatile private var foo = 3
fn annotation(p: &mut Parser) {
  p.eat(T![@]);
  p.expect(T![ident]);

  if p.at(T!['(']) {
    super::expr::call_paren_expr(p);
  }
}

fn package_item(p: &mut Parser, m: Marker) {
  p.eat(T![package]);

  let path = p.start();

  loop {
    match p.current() {
      // test ok
      // package foo
      T![ident] => p.eat(T![ident]),

      // test err
      // package 3
      _ => {
        path.abandon(p);
        p.error(format!("expected ident, got {:?}", p.current()));
        p.recover_until(T![nl]);
        m.abandon(p);
        return;
      }
    }

    match p.current() {
      // test ok
      // package foo.bar
      T![.] => {
        p.eat(T![.]);
        continue;
      }

      // test ok
      // package foo {
      //   class Bar {}
      // }
      T!['{'] => {
        path.complete(p, PATH);
        block_items(p);
        m.complete(p, PACKAGE);
        return;
      }

      T![nl] | EOF => {
        path.complete(p, PATH);
        m.complete(p, PACKAGE);
        return;
      }

      // test err
      // package foo 3
      _ => {
        path.abandon(p);
        p.error(format!("expected dot, got {:?}", p.current()));
        p.recover_until(T![nl]);
        m.abandon(p);
        return;
      }
    }
  }
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
// import foo.bar.{
//   qux,
//   quz
// }
fn import_list(p: &mut Parser, m: Marker) {
  p.eat(T!['{']);
  p.eat_newlines();

  loop {
    {
      let m = p.start();
      p.expect(T![ident]);

      // test ok
      // import foo.{ bar => baz }
      if p.at(T![=>]) {
        p.eat(T![=>]);
        p.expect(T![ident]);

        m.complete(p, IMPORT_SELECTOR_RENAME);
      } else {
        m.complete(p, IMPORT_SELECTOR_ID);
      }
    }
    p.eat_newlines();

    if p.current() == T![,] {
      p.eat(T![,]);

      // test ok
      // import foo.bar.{
      //   baz
      //   ,
      //   blah
      // }
      p.eat_newlines();
    } else {
      p.expect(T!['}']);
      m.complete(p, IMPORT_SELECTORS);
      break;
    }
  }
}

// test ok
// class Foo() {}
// trait Bar extends Blah {}
fn class_def(p: &mut Parser, m: Marker) {
  // test ok
  // case class Foo() {}
  // case object Foo {}
  if p.current() == T![case] {
    p.eat(T![case]);
  }

  let kind = match p.current() {
    // test ok
    // object Foo
    T![object] => {
      p.eat(T![object]);
      OBJECT_DEF
    }

    // test ok
    // class Foo
    T![class] => {
      p.eat(T![class]);
      CLASS_DEF
    }

    // test ok
    // trait Bar
    T![trait] => {
      p.eat(T![trait]);
      TRAIT_DEF
    }

    _ => {
      p.error("expected class or object");
      m.abandon(p);
      return;
    }
  };

  p.expect(T![ident]);

  // test ok
  // class Foo private {}
  // class Bar protected {}
  match p.current() {
    T![private] | T![protected] => {
      p.bump();
      p.eat_newlines();
    }
    _ => {}
  }

  // test ok
  // class Foo {}
  // class Foo() {}
  if p.current() == T!['('] {
    fun_params(p, true);
  }

  // test ok
  // class Foo
  //   extends Bar {}
  if p.at(T![nl]) && p.peek() == T![extends] {
    // NB: Don't eat multiple newlines, as that'll break parsing of blank classes
    // like `class Foo`.
    p.eat(T![nl]);
  }

  // test ok
  // class Foo extends AnyVal {}
  if p.current() == T![extends] {
    p.eat(T![extends]);

    extends_item(p);
  }

  // test ok
  // class foo extends AnyVal with Bar
  // class foo extends AnyVal with Bar with Baz
  loop {
    if p.at(T![with]) {
      p.eat(T![with]);
      super::type_expr::type_expr(p);
    } else if p.at(T![nl]) && p.peek() == T![with] {
      // test ok
      // class Foo
      //   extends Foo
      //   with Bar
      //   with Baz
      p.eat(T![nl]);
      p.eat(T![with]);
      super::type_expr::type_expr(p);
    } else {
      break;
    }
  }

  // test ok
  // class Foo
  // class Foo {}
  if p.current() == T!['{'] {
    item_body(p);
  }

  m.complete(p, kind);
}

// test ok
// class Foo extends foo.bar.Baz[Int, String](2, 3) {}
fn extends_item(p: &mut Parser) {
  p.expect(T![ident]);

  // test ok
  // class Foo extends foo.bar.Baz {}
  while p.at(T![.]) {
    p.bump();
    p.expect(T![ident]);
  }

  // test ok
  // class Foo extends Seq[Int] {}
  if p.at(T!['[']) {
    let m = p.start();
    super::type_expr::type_args(p, T!['['], T![']']);
    m.complete(p, TYPE_ARGS);
  }

  // test ok
  // class Foo extends Bar(2, 3) {}
  if p.at(T!['(']) {
    super::expr::call_paren_expr(p);
  }
}

fn item_body(p: &mut Parser) {
  let m = p.start();
  p.eat(T!['{']);

  items(p, BlockTerminator::Brace);

  m.complete(p, ITEM_BODY);
}

// test ok
// def bar
// def foo = 3
fn fun_def(p: &mut Parser, m: Marker) {
  p.eat(T![def]);
  fun_sig(p);

  if p.at(T![=]) {
    p.eat(T![=]);

    // test ok
    // def foo =
    //   2
    if p.eat_newlines() >= 2 {
      // test err
      // def foo =
      //
      //   3
      p.error("expected expr");
    }

    expr::expr(p);
  }

  m.complete(p, FUN_DEF);
}

// test ok
// def foo(a: String, b: String): List[Int] = 2
fn fun_sig(p: &mut Parser) {
  let m = p.start();
  p.expect(T![ident]);

  if p.at(T!['[']) {
    // test ok
    // def foo[A <: Int](a: A) = 3
    let m = p.start();
    super::type_expr::type_params(p, T!['['], T![']']);
    m.complete(p, TYPE_PARAMS);
  }

  // test ok
  // def foo(a: String) = 2
  // def bar(a: String)(b: String) = 3
  while p.at(T!['(']) {
    fun_params(p, false);
  }

  // test ok
  // def bar: Int = 2
  if p.at(T![:]) {
    p.eat(T![:]);
    super::type_expr::type_expr(p);
  }

  m.complete(p, FUN_SIG);
}

fn fun_params(p: &mut Parser, is_class: bool) {
  let m = p.start();
  p.eat(T!['(']);

  // test ok
  // def foo(
  //   a: Int
  // ) = 3
  //
  // test ok
  // def foo(
  //   implicit a: Int
  // ) = 3
  p.eat_newlines();

  // test ok
  // def foo(implicit a: Int) = 3
  if p.at(T![implicit]) {
    p.eat(T![implicit]);
  }

  // test ok
  // def foo() = 3
  if p.at(T![')']) {
    p.eat(T![')']);
    m.complete(p, FUN_PARAMS);
    return;
  }

  loop {
    // test ok
    // class Foo(val a: Int, val b: Int) {}
    if is_class && p.at(T![val]) {
      p.eat(T![val]);
    }

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

  // test ok
  // def bar(a: Int = 3) = a + 1
  if p.at(T![=]) {
    p.eat(T![=]);
    super::expr::expr(p);
  }

  m.complete(p, FUN_PARAM);
}

// test ok
// val foo = 3
fn val_def(p: &mut Parser, m: Marker) {
  let kind = match p.current() {
    T![val] => VAL_DEF,
    T![var] => VAR_DEF,
    _ => panic!("expected val or var"),
  };
  p.bump();
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

  m.complete(p, kind);
}

pub fn case_item(p: &mut Parser, m: Marker) {
  p.expect(T![case]);
  super::pattern::pattern(p);

  // test ok
  // case _ if true =>
  if p.at(T![if]) {
    let m = p.start();
    p.eat(T![if]);
    super::expr::expr_no_fat_arrow(p);
    m.complete(p, GUARD);
  }

  p.expect(T![=>]);

  p.eat_newlines();

  // An expression after the `=>` is optional.
  //
  // test ok
  // case 3 =>
  if !(p.at(T!['}']) || p.at(T![nl]) || p.at(EOF)) {
    let m = p.start();
    items(p, BlockTerminator::Case);
    m.complete(p, BLOCK);
  }

  m.complete(p, CASE_ITEM);
}
