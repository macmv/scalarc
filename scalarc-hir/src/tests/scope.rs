use std::fmt;

use crate::{scope::Scope, Definition, HirDatabase};
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

fn defs_at(src: &str, expected: Expect) {
  let cursor = src.find("@@").unwrap();
  let src = format!("{}{}", &src[..cursor], &src[cursor + 2..]);

  let db = new_db(&src);
  let actual = db.defs_at_index(FileId::temp_new(), TextSize::from(cursor as u32));
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
    actual_src.insert_str(r.pos.range.end().into(), "@");
    actual_src.insert_str(r.pos.range.start().into(), "@");
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
    let item_id_map = self.db.item_id_map(FileId::temp_new());
    let item = item_id_map.get_erased(self.item.item_id);

    f.debug_struct("Scope")
      .field("item", &item)
      .field("declarations", &self.child(&self.item.declarations))
      .finish()
  }
}

impl fmt::Debug for DebugUtil<'_, '_, Vec<(String, Definition)>> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_map().entries(self.item.iter().map(|&(ref k, ref v)| (k, self.child(v)))).finish()
  }
}

impl fmt::Debug for DebugUtil<'_, '_, Vec<Definition>> {
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

impl fmt::Debug for DebugUtil<'_, '_, Definition> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Definition")
      // .field("pos", &self.0.pos.range)
      .field("name", &self.item.name.as_str())
      .field("kind", &self.item.kind)
      .finish()
  }
}

#[test]
fn scopes_of_example() {
  scopes_of(
    r#"
    val foo = 3
    val bar = 4
    val baz = 5
    "#,
    expect![@r#"
      [
        Scope {
          item: SyntaxNodePtr {
            kind: SOURCE_FILE,
            range: 0..49,
          },
          declarations: {
            "foo": Definition {
              name: "foo",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
            "bar": Definition {
              name: "bar",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
            "baz": Definition {
              name: "baz",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
          },
        },
      ]"#],
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
    expect![@r#"
      [
        Scope {
          item: SyntaxNodePtr {
            kind: SOURCE_FILE,
            range: 0..87,
          },
          declarations: {
            "foo": Definition {
              name: "foo",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
            "bar": Definition {
              name: "bar",
              kind: Val(
                None,
              ),
            },
            "baz": Definition {
              name: "baz",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
          },
        },
        Scope {
          item: SyntaxNodePtr {
            kind: BLOCK_EXPR,
            range: 31..70,
          },
          declarations: {
            "a": Definition {
              name: "a",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
            "b": Definition {
              name: "b",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
          },
        },
      ]"#],
  );
}

#[test]
fn definitions_at() {
  defs_at(
    r#"
    val foo = 3
    val bar = {
      val a = 6
      @@
      val b = 7
    }
    val baz = 5
    "#,
    expect![@r#"
      [
        Definition {
          name: "a",
          kind: Val(
            Some(
              Type(scala.Int),
            ),
          ),
        },
      ]"#],
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
      None
    "#],
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
      Definition {
        name: "a",
        kind: Val(
          Some(
            Type(scala.Int),
          ),
        ),
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
      Definition {
        name: "foo",
        kind: Def(
          Signature((a: Int)),
        ),
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
              kind: Class,
            },
          },
        },
        Scope {
          item: SyntaxNodePtr {
            kind: CLASS_DEF,
            range: 5..59,
          },
          declarations: {
            "b": Definition {
              name: "b",
              kind: Val(
                Some(
                  Type(String),
                ),
              ),
            },
          },
        },
      ]"#],
  );
}

#[test]
fn class_def() {
  defs_at(
    r#"
    class Foo(val a: Int) {}
    @@
    "#,
    expect![@r#"
      [
        Definition {
          name: "Foo",
          kind: Class,
        },
      ]"#],
  );

  defs_at(
    r#"
    class Foo(a: Int) { @@ }
    "#,
    expect![@"[]"],
  );

  defs_at(
    r#"
    class Foo(a: Int) {
      val b: String
      @@
    }
    "#,
    expect![@r#"
      [
        Definition {
          name: "b",
          kind: Val(
            Some(
              Type(String),
            ),
          ),
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
    expect![@r#"
      [
        Scope {
          item: SyntaxNodePtr {
            kind: SOURCE_FILE,
            range: 0..51,
          },
          declarations: {
            "foo": Definition {
              name: "foo",
              kind: Def(
                Signature((a: Int)),
              ),
            },
          },
        },
        Scope {
          item: SyntaxNodePtr {
            kind: BLOCK_EXPR,
            range: 23..50,
          },
          declarations: {
            "b": Definition {
              name: "b",
              kind: Val(
                Some(
                  Type(String),
                ),
              ),
            },
          },
        },
      ]"#],
  );
}

#[test]
fn fun_def() {
  defs_at(
    r#"
    def foo(a: Int) = 3
    @@
    "#,
    expect![@r#"
      [
        Definition {
          name: "foo",
          kind: Def(
            Signature((a: Int)),
          ),
        },
      ]"#],
  );

  defs_at(
    r#"
    def foo(a: Int) = @@a
    "#,
    expect![@"[]"],
  );

  defs_at(
    r#"
    def foo(a: Int) = a@@
    "#,
    expect![@r#"
      [
        Definition {
          name: "foo",
          kind: Def(
            Signature((a: Int)),
          ),
        },
      ]"#],
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
    expect![@r#"
      [
        Scope {
          item: SyntaxNodePtr {
            kind: SOURCE_FILE,
            range: 0..308,
          },
          declarations: {
            "a": Definition {
              name: "a",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
          },
        },
        Scope {
          item: SyntaxNodePtr {
            kind: BLOCK_EXPR,
            range: 20..43,
          },
          declarations: {
            "b": Definition {
              name: "b",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
          },
        },
        Scope {
          item: SyntaxNodePtr {
            kind: BLOCK_EXPR,
            range: 61..84,
          },
          declarations: {
            "c": Definition {
              name: "c",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
          },
        },
        Scope {
          item: SyntaxNodePtr {
            kind: BLOCK_EXPR,
            range: 101..124,
          },
          declarations: {
            "d": Definition {
              name: "d",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
          },
        },
        Scope {
          item: SyntaxNodePtr {
            kind: BLOCK_EXPR,
            range: 275..306,
          },
          declarations: {
            "h": Definition {
              name: "h",
              kind: Val(
                Some(
                  Type(scala.Int),
                ),
              ),
            },
          },
        },
      ]"#],
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
        @a@
      }
    "#],
  );
}
