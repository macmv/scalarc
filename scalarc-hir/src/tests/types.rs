use scalarc_source::FileId;

use scalarc_syntax::TextSize;
use scalarc_test::{expect, Expect};

use crate::HirDatabase;

use super::new_db;

fn type_at(src: &str, expected: Expect) {
  let cursor = src.find("@@").unwrap();
  let src = format!("{}{}", &src[..cursor], &src[cursor + 2..]);

  let db = new_db(&src);
  let actual = db.type_at(FileId::temp_new(), TextSize::from(cursor as u32));

  let actual = match actual {
    Some(ty) => ty.to_string(),
    None => "no type".to_string(),
  };

  expected.assert_eq(&actual);
}

#[test]
fn simple_type_at() {
  type_at("@@3", expect![@"scala.Int"]);
  type_at("3@@", expect![@"scala.Int"]);
  type_at("3.2@@", expect![@"scala.Float"]);
  type_at("(3)@@", expect![@"scala.Int"]);

  type_at("@@ 3", expect![@"no type"]);
}

#[test]
fn tuples() { type_at("(2, 3)@@", expect![@"(scala.Int, scala.Int)"]); }

#[test]
fn type_of_val_def() {
  type_at(
    r#"
    val foo@@ = 2
    "#,
    expect![@"scala.Int"],
  );

  type_at(
    r#"
    val foo@@ = { 2 }
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_object() {
  type_at(
    r#"
    object Foo {
      def bar = 3
    }

    val foo@@ = Foo
    "#,
    expect![@"$Foo"],
  );

  type_at(
    r#"
    object Foo {
      def bar = 3
    }

    val foo@@ = Foo.bar
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_val_ref() {
  type_at(
    r#"
    val foo = 2
    foo@@
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_nested() {
  type_at(
    r#"
    class Foo() {
      val foo = 2
      foo@@
    }
    "#,
    expect![@"scala.Int"],
  );

  type_at(
    r#"
    class Foo() {
      val foo = 2
      val bar = foo@@
    }
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_call() {
  type_at(
    r#"
    object Foo {
      def bar = 3
    }

    val foo@@ = Foo.bar
    "#,
    expect![@"scala.Int"],
  );

  type_at(
    r#"
    object Foo {
      def bar(a: Int, b: String) = 3
    }

    val foo@@ = Foo.bar
    "#,
    expect![@"(scala.Int, scala.String) => scala.Int"],
  );
  type_at(
    r#"
    object Foo {
      def bar(a: Int)(b: String) = 3
    }

    val foo@@ = Foo.bar
    "#,
    expect![@"(scala.Int) => (scala.String) => scala.Int"],
  );

  type_at(
    r#"
    object Foo {
      def foo = Bar.foo
    }
    object Bar {
      def foo = Foo.foo@@
    }
    "#,
    expect![@"no type"],
  );
}

#[test]
fn type_of_stdlib() {
  type_at(
    r#"
    val foo = 2 + 3
    foo@@
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_class() {
  type_at(
    r#"
    class Foo {
      val x = 2
    }

    val foo = new Foo()
    foo@@
    "#,
    expect![@"Foo"],
  );
}

#[test]
fn type_of_object_vs_class() {
  type_at(
    r#"
    object Foo {
      val x = 2
    }

    class Foo {
      val y = 2
    }

    Foo.x@@
    "#,
    expect![@"scala.Int"],
  );

  type_at(
    r#"
    object Foo {
      val x = 2
    }

    class Foo {
      val y = 2
    }

    new Foo().y@@
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_param() {
  type_at(
    r#"
    def foo(x: Int) = x@@
    "#,
    expect![@"scala.Int"],
  );

  type_at(
    r#"
    def foo(x: Int) = {
      x@@
    }
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_import() {
  type_at(
    r#"
    package foo.bar

    object Foo {
      val x = 2
    }

    import foo.bar.{ Foo => Baz }

    Baz.x@@
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_match() {
  type_at(
    r#"
    package foo.bar

    val foo = 0 match {
      case _ => 3
    }

    foo@@
    "#,
    expect![@"scala.Int"],
  );
}

#[test]
fn type_of_nested_val() {
  type_at(
    r#"
    val x = 3

    val y = {
      x
    }

    y@@
   "#,
    expect![@"scala.Int"],
  );

  type_at(
    r#"
    val y = {
      x
    }

    // FIXME: This causes recursion in salsa.
    // val x = 3

    y@@
   "#,
    expect![@"unknown"],
  );
}
