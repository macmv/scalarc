use std::fmt;

use crate::{
  scope::Scope, AnyDefinition, DefinitionMap, GlobalDefinition, HirDatabase, HirDefinition,
  HirDefinitionId,
};
use la_arena::Arena;
use scalarc_source::FileId;
use scalarc_syntax::TextSize;
use scalarc_test::{expect, Expect};

use super::new_db;

fn scopes_of(src: &str, expected: Expect) {
  let db = new_db(src);
  let actual = db.scopes_of(FileId::temp_new());
  expected.assert_eq(&DebugUtil { db: &db, item: &actual.scopes }.to_string());
}

fn def_map(src: &str, expected: Expect) {
  let db = new_db(&src);
  let actual = db.definitions_for_file(FileId::temp_new());
  expected.assert_eq(&DebugUtil { db: &db, item: &actual }.to_string());
}

fn def_at(src: &str, expected: Expect) {
  let cursor = src.find("@@").unwrap();
  let src = format!("{}{}", &src[..cursor], &src[cursor + 2..]);

  let db = new_db(&src);
  let actual = db.def_at_index(FileId::temp_new(), TextSize::from(cursor as u32));
  expected.assert_eq(&DebugUtil { db: &db, item: &actual }.to_string());
}

fn refs_to(src: &str, expected: Expect) {
  let cursor = src.find("@@").unwrap();
  let src = format!("{}{}", &src[..cursor], &src[cursor + 2..]);

  let db = new_db(&src);
  let actual = db.references_to(FileId::temp_new(), TextSize::from(cursor as u32));

  let mut actual_src = src.to_string();
  for r in actual.iter().rev() {
    actual_src.insert(r.pos.range.end().into(), '@');
    actual_src.insert(r.pos.range.start().into(), '@');
  }

  let actual_src = actual_src
    .trim_start_matches(|c| c == '\n')
    .lines()
    .map(|l| l.strip_prefix("    ").unwrap_or(l))
    .collect::<Vec<_>>()
    .join("\n");

  expected.assert_eq(&actual_src);
}

fn indent(s: String) -> String { s.replace("    ", "  ") }

struct DebugUtil<'db, 'it: 'db, T> {
  db:   &'db dyn HirDatabase,
  item: &'it T,
}

impl<'db, T> DebugUtil<'db, '_, T> {
  fn child<'it, U>(&self, item: &'it U) -> DebugUtil<'db, 'it, U> {
    DebugUtil { db: self.db, item }
  }
}

impl<'db, 'it, T> fmt::Display for DebugUtil<'db, 'it, T>
where
  DebugUtil<'db, 'it, T>: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", indent(format!("{:#?}", self)))
  }
}

impl fmt::Debug for DebugUtil<'_, '_, Arena<Scope>> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.item.iter().map(|(_, s)| self.child(s))).finish()
  }
}

impl fmt::Debug for DebugUtil<'_, '_, Scope> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let ast_id_map = self.db.ast_id_map(FileId::temp_new());
    let item = ast_id_map.get_erased(self.item.ast_id);

    f.debug_struct("Scope")
      .field("item", &item)
      .field("declarations", &self.child(&self.item.declarations))
      .finish()
  }
}

impl fmt::Debug for DebugUtil<'_, '_, DefinitionMap> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_map().entries(self.item.items.iter().map(|(k, v)| (k, self.child(v)))).finish()
  }
}

impl fmt::Debug for DebugUtil<'_, '_, Vec<(String, GlobalDefinition)>> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_map().entries(self.item.iter().map(|(k, v)| (k, self.child(v)))).finish()
  }
}

impl fmt::Debug for DebugUtil<'_, '_, Vec<GlobalDefinition>> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.item.iter().map(|d| self.child(d))).finish()
  }
}

impl<'db, 'it, T> fmt::Debug for DebugUtil<'db, 'it, Option<T>>
where
  DebugUtil<'db, 'it, T>: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.item {
      Some(v) => write!(f, "{:#?}", &self.child(v)),
      None => writeln!(f, "None"),
    }
  }
}

impl fmt::Debug for DebugUtil<'_, '_, AnyDefinition> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.item {
      AnyDefinition::Global(d) => self.child(d).fmt(f),
      AnyDefinition::Hir(d) => self.child(d).fmt(f),
    }
  }
}

impl fmt::Debug for DebugUtil<'_, '_, GlobalDefinition> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let item_id_map = self.db.ast_id_map(FileId::temp_new());
    let item = item_id_map.get_erased(self.item.ast_id);

    f.debug_struct("Definition")
      .field("name", &self.item.name.as_str())
      .field("kind", &self.item.kind)
      .field("item", &item)
      .finish()
  }
}

impl fmt::Debug for DebugUtil<'_, '_, HirDefinition> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let source_map = self.db.hir_source_map_for_block(self.item.block_id);
    let item = match self.item.id {
      HirDefinitionId::Stmt(s) => source_map.stmt_syntax(s).unwrap(),
      HirDefinitionId::Param(_) => unreachable!(),
      HirDefinitionId::Import(_) => unreachable!(),
    };

    f.debug_struct("LocalDefinition")
      .field("name", &self.item.name.as_str())
      .field("kind", &self.item.kind)
      .field("item", &item)
      .finish()
  }
}

#[test]
fn simple_example() {
  scopes_of(
    r#"
    class Foo {}
    class Bar {}
    class Baz {}
    "#,
    expect![@r#"
      [
        Scope {
          item: SyntaxNodePtr {
            kind: SOURCE_FILE,
            range: 0..52,
          },
          declarations: {
            "Foo": Definition {
              name: "Foo",
              kind: Class(
                Some(
                  AstId {
                    raw: Idx::<Scala>>(4),
                    phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::ItemBody>,
                  },
                ),
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 5..17,
              },
            },
            "Bar": Definition {
              name: "Bar",
              kind: Class(
                Some(
                  AstId {
                    raw: Idx::<Scala>>(5),
                    phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::ItemBody>,
                  },
                ),
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 22..34,
              },
            },
            "Baz": Definition {
              name: "Baz",
              kind: Class(
                Some(
                  AstId {
                    raw: Idx::<Scala>>(6),
                    phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::ItemBody>,
                  },
                ),
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 39..51,
              },
            },
          },
        },
      ]"#],
  );

  scopes_of(
    r#"
    class Foo {}
    class Bar {
      class A
      class B
    }
    class Baz {}
    "#,
    expect![@r#"
      [
        Scope {
          item: SyntaxNodePtr {
            kind: SOURCE_FILE,
            range: 0..85,
          },
          declarations: {
            "Foo": Definition {
              name: "Foo",
              kind: Class(
                Some(
                  AstId {
                    raw: Idx::<Scala>>(4),
                    phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::ItemBody>,
                  },
                ),
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 5..17,
              },
            },
            "Bar": Definition {
              name: "Bar",
              kind: Class(
                Some(
                  AstId {
                    raw: Idx::<Scala>>(5),
                    phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::ItemBody>,
                  },
                ),
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 22..67,
              },
            },
            "Baz": Definition {
              name: "Baz",
              kind: Class(
                Some(
                  AstId {
                    raw: Idx::<Scala>>(6),
                    phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::ItemBody>,
                  },
                ),
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 72..84,
              },
            },
          },
        },
        Scope {
          item: SyntaxNodePtr {
            kind: ITEM_BODY,
            range: 32..67,
          },
          declarations: {
            "A": Definition {
              name: "A",
              kind: Class(
                None,
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 40..47,
              },
            },
            "B": Definition {
              name: "B",
              kind: Class(
                None,
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 54..61,
              },
            },
          },
        },
      ]"#],
  );
}

#[test]
fn vals_dont_make_scopes() {
  scopes_of(
    r#"
    val foo = 3
    val bar = 4
    val baz = 5
    "#,
    expect![@"[]"],
  );

  scopes_of(
    r#"
    val foo = 3
    val bar = {
      val a = 6
      val b = 7
    }
    val baz = 5
    "#,
    expect![@"[]"],
  );
}

#[test]
fn definition_at() {
  def_at(
    r#"
    val foo = 3
    val bar = {
      val a@@ = 6
      val b = 7
    }
    val baz = 5
    "#,
    expect![@r#"
      LocalDefinition {
        name: "a",
        kind: Val(
          None,
        ),
        item: AstPtr {
          ptr: SyntaxNodePtr {
            kind: VAL_DEF,
            range: 39..48,
          },
          _phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::Item>,
        },
      }"#],
  );

  def_at(
    r#"
    val foo = 3
    val bar = {
      val a = 6
      a@@
      val b = 7
    }
    val baz = 5
    "#,
    expect![@r#"
      LocalDefinition {
        name: "a",
        kind: Val(
          None,
        ),
        item: AstPtr {
          ptr: SyntaxNodePtr {
            kind: VAL_DEF,
            range: 39..48,
          },
          _phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::Item>,
        },
      }"#],
  );
}

#[test]
fn def_sigs() {
  def_at(
    r#"
    def foo(a: Int): String = "hi"
    foo@@
    "#,
    expect![@r#"
      LocalDefinition {
        name: "foo",
        kind: Def(
          Signature(),
        ),
        item: AstPtr {
          ptr: SyntaxNodePtr {
            kind: FUN_DEF,
            range: 5..35,
          },
          _phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::Item>,
        },
      }"#],
  );
}

#[test]
fn class_scopes() {
  scopes_of(
    r#"
    class Foo(a: Int) {
      val b: String
      @@
    }
    "#,
    expect![@r#"
      [
        Scope {
          item: SyntaxNodePtr {
            kind: SOURCE_FILE,
            range: 0..60,
          },
          declarations: {
            "Foo": Definition {
              name: "Foo",
              kind: Class(
                Some(
                  AstId {
                    raw: Idx::<Scala>>(3),
                    phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::ItemBody>,
                  },
                ),
                Normal,
              ),
              item: SyntaxNodePtr {
                kind: CLASS_DEF,
                range: 5..59,
              },
            },
          },
        },
      ]"#],
  );
}

#[test]
fn fun_scopes() {
  scopes_of(
    r#"
    def foo(a: Int) = {
      val b: String
    }
    "#,
    expect![@"[]"],
  );
}

#[test]
fn nested_scopes() {
  scopes_of(
    r#"
    val a = 3

    {
      val b = 3
    }

    if (a == 4) {
      val c = 3
    } else if (a < 0) {
      val d = 5
    }

    a match {
      case 3 =>
        val e = 8
        e
      case _ => println(a)
    }

    println {
      val g = 5
      g
    }

    println({
      val h = 6
      h
    })
    "#,
    expect![@"[]"],
  );
}

#[test]
fn refs_to_val() {
  refs_to(
    r#"
    val a = 3
    a@@
    a + b
    println(a)

    if (a > 3) {
      val a = 4
      a
    }
    "#,
    expect![@r#"
      val a = 3
      @a@
      @a@ + b
      println(@a@)

      if (@a@ > 3) {
        val a = 4
        a
      }
    "#],
  );
}

#[test]
fn refs_hovering_on_val() {
  refs_to(
    r#"
    val a@@ = 3
    a
    a + b
    "#,
    expect![@r#"
      val a = 3
      @a@
      @a@ + b
    "#],
  );
}

#[test]
fn refs_to_def() {
  refs_to(
    r#"
    def a@@ = 3
    a
    "#,
    expect![@r#"
      def a = 3
      @a@
    "#],
  );
}

#[test]
fn refs_to_object() {
  refs_to(
    r#"
    object A {}
    A@@
    A + b
    println(A)

    if (A > 3) {
      val A = 4
      A
    }
    "#,
    expect![@r#"
      object A {}
      A
      A + b
      println(A)

      if (A > 3) {
        val A = 4
        A
      }
    "#],
  );
}

#[test]
fn def_map_packages() {
  def_map(
    r#"
    package foo.bar
    val baz = 3
    "#,
    expect![@"{}"],
  );
}
