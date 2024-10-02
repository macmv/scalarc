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
        x()
      }
   "#],
  );
}

#[test]
fn goto_constructor() {
  goto_definition(
    r#"
    object Foo {
      def apply() = new Foo()
    }
    class Foo()

    class Foo {
      import Foo
      Foo|()
    }
    "#,
    expect![@r#"
      @object Foo {
        def apply() = new Foo()
      }@
      class Foo()

      class Foo {
        import Foo
        Foo()
      }
    "#],
  );

  println!("======");
  goto_definition(
    r#"
    object Foo {
      def apply() = new Foo()
    }
    class Foo()

    class Foo {
      import Foo
      new Foo|()
    }
    "#,
    expect![@r#"
      object Foo {
        def apply() = new Foo()
      }
      @class Foo()@

      class Foo {
        import Foo
        new Foo()
      }
    "#],
  );
}
