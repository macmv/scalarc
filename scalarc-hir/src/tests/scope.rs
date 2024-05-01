use std::fmt;

use crate::{scope::Scope, HirDatabase};
use la_arena::Arena;
use scalarc_source::{FileId, SourceDatabase};
use scalarc_test::{expect, Expect};

use super::TestDB;

fn new_db(content: &str) -> TestDB {
  let mut db = TestDB::default();
  let file = FileId::temp_new();
  db.set_file_text(file, content.into());
  db
}

fn scopes_of(src: &str, expected: Expect) {
  let db = new_db(src);
  let actual = db.scopes_of(FileId::temp_new());
  expected.assert_eq(&DebugScopes(&actual).to_string());
}

struct DebugScopes<'a>(&'a Arena<Scope>);
impl fmt::Display for DebugScopes<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[\n")?;
    for (_, s) in self.0.iter() {
      writeln!(f, "  Scope {{")?;
      writeln!(f, "    range: {:?},", s.range)?;
      writeln!(f, "    declarations: {{")?;
      for (name, def) in &s.declarations {
        write!(f, "      ")?;
        write!(f, "{:?}: Definition {{ ", name)?;
        write!(f, "pos: {:?}, ", def.pos.range)?;
        write!(f, "kind: {:?} ", def.kind)?;
        writeln!(f, "}}")?;
      }
      writeln!(f, "    }}")?;
      writeln!(f, "  }}")?;
    }
    write!(f, "]\n")
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
          range: 0..49,
          declarations: {
            "foo": Definition { pos: 9..12, kind: Local(Val) }
            "bar": Definition { pos: 25..28, kind: Local(Val) }
            "baz": Definition { pos: 41..44, kind: Local(Val) }
          }
        }
      ]
    "#],
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
          range: 0..87,
          declarations: {
            "foo": Definition { pos: 9..12, kind: Local(Val) }
            "bar": Definition { pos: 25..28, kind: Local(Val) }
            "baz": Definition { pos: 79..82, kind: Local(Val) }
          }
        }
        Scope {
          range: 31..70,
          declarations: {
            "a": Definition { pos: 43..44, kind: Local(Val) }
            "b": Definition { pos: 59..60, kind: Local(Val) }
          }
        }
      ]
    "#],
  );
}
