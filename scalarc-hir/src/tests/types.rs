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
  // type_at(
  //   r#"
  //   object Foo {
  //     def bar = 3
  //   }

  //   val foo@@ = Foo
  //   "#,
  //   expect![@"Foo"],
  // );

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
fn type_of_call() {
  type_at(
    r#"
    def foo(a: scala.Int): scala.Float = 3.0
    foo(2)@@
    "#,
    expect![@"no type"],
  );
}
