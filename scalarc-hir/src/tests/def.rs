use scalarc_source::{FileId, SourceDatabase};
use scalarc_test::{expect, Expect};

use crate::{DefinitionKey, HirDatabase, HirDefinitionId};

#[track_caller]
fn def_at(src: &str, expect: Expect) {
  let src = src
    .trim_start_matches(|c| c == '\n')
    .lines()
    .map(|l| l.strip_prefix("    ").unwrap_or(l))
    .collect::<Vec<_>>()
    .join("\n");

  let cursor = src.find('|').unwrap();
  let real_src = src[..cursor as usize].to_string() + &src[cursor as usize + 1..];

  let file = FileId::temp_new();
  let db = super::new_db(&real_src);

  let def = db.def_at_index(file, (cursor as u32).into()).expect("no definition found");

  let span = match def {
    crate::AnyDefinition::Hir(ref d) => {
      let ast = db.parse(file);
      let source_map = db.hir_source_map_for_block(d.block_id);
      let item = match d.id {
        HirDefinitionId::Stmt(s) => source_map.stmt_syntax(s).unwrap().to_node(&ast),
        HirDefinitionId::Param(s) => {
          source_map.param_syntax(s).unwrap().to_node(&ast.syntax_node())
        }
        HirDefinitionId::Pattern(s) => source_map.pattern_syntax(s).unwrap().to_node(&ast),
        HirDefinitionId::Import(id) => {
          let import = &db.hir_ast_for_block(d.block_id).imports[id];
          let target = db.file_target(file).unwrap();
          let def = db
            .definition_for_key(target, DefinitionKey::Instance(import.path.clone()))
            .expect("no definition found for imported path");

          assert_eq!(def.file_id, file, "import points to a different file");

          db.ast_id_map(def.file_id).get_erased(def.ast_id).to_node(&ast.syntax_node())
        }
      };
      item.text_range()
    }
    crate::AnyDefinition::Global(ref d) => {
      let item = db.ast_id_map(d.file_id).get_erased(d.ast_id);
      item.text_range()
    }
  };

  let actual_start = u32::from(span.start()) as usize;
  let actual_end = u32::from(span.end()) as usize;

  let expected = format!(
    "{}@{}@{}",
    &real_src[..actual_start],
    &real_src[actual_start..actual_end],
    &real_src[actual_end..],
  );

  expect.assert_eq(&expected);
}

#[test]
fn it_works() {
  def_at(
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
fn nested_blocks_work() {
  def_at(
    r#"
    class Foo {
      def x = 3
      def y = {
        x|()
      }
    }
    "#,
    expect![@r#"
      class Foo {
        @def x = 3@
        def y = {
          x()
        }
      }
    "#],
  );
}

#[test]
fn params_work() {
  def_at(
    r#"
    class Foo {
      def x(foo: Int) = foo|
    }
    "#,
    expect![@r#"
      class Foo {
        def x(@foo: Int@) = foo
      }
    "#],
  );

  def_at(
    r#"
    class Foo {
      def x(foo: Int) = {
        foo|
      }
    }
    "#,
    expect![@r#"
      class Foo {
        def x(@foo: Int@) = {
          foo
        }
      }
    "#],
  );
}

#[test]
fn class_params_work() {
  def_at(
    r#"
    class Foo(x: Int) {
      x|()
    }
    "#,
    expect![@r#"
      class Foo(@x: Int@) {
        x()
      }
    "#],
  );
}

#[test]
fn imports_work() {
  def_at(
    r#"
    package foo.bar

    class Bar {
      def x = 3
    }

    class Foo {
      import foo.bar.Bar

      Bar|
    }
    "#,
    expect![@r#"
      package foo.bar

      @class Bar {
        def x = 3
      }@

      class Foo {
        import foo.bar.Bar

        Bar
      }
    "#],
  );

  def_at(
    r#"
    package foo.bar

    class Bar {
      def x = 3
    }

    class Foo {
      import foo.bar.{ Bar }

      Bar|
    }
    "#,
    expect![@r#"
      package foo.bar

      @class Bar {
        def x = 3
      }@

      class Foo {
        import foo.bar.{ Bar }

        Bar
      }
    "#],
  );
}

#[test]
fn import_renames() {
  def_at(
    r#"
    package foo.bar

    class Bar {
      def x = 3
    }

    class Foo {
      import foo.bar.{ Bar => Baz }

      Baz|
    }
    "#,
    expect![@r#"
      package foo.bar

      @class Bar {
        def x = 3
      }@

      class Foo {
        import foo.bar.{ Bar => Baz }

        Baz
      }
    "#],
  );
}

#[test]
fn import_all() {
  def_at(
    r#"
    package foo.bar

    object Bar {
      def x = 3
    }

    class Foo {
      import foo.bar._

      Bar|
    }
    "#,
    expect![@r#"
      package foo.bar

      @object Bar {
        def x = 3
      }@

      class Foo {
        import foo.bar._

        Bar
      }
    "#],
  );
}

#[test]
fn pattern_bindings() {
  def_at(
    r#"
    0 match {
      case x => x|
    }
    "#,
    expect![@r#"
      0 match {
        case @x@ => x
      }
    "#],
  );
}
