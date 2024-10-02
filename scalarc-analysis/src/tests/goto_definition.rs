use scalarc_test::expect;

use super::goto_definition;

#[test]
fn it_works() {
  goto_definition(
    r#"
    class Foo {
      def x = 3
      x|
    }
   "#,
    expect![@r#"
      class Foo {
        @def x = 3@
        x
      }
   "#],
  );
}

#[test]
fn calls_work() {
  goto_definition(
    r#"
    class Foo {
      def x = 3
      x|()
    }
   "#,
    expect![@r#"
      class Foo {
        @def x = 3@
        x
      }
   "#],
  );
}
