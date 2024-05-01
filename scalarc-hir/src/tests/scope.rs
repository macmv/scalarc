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
          declarations: {
            "baz": Definition { pos: 41..44, kind: Local(Val) }
            "bar": Definition { pos: 25..28, kind: Local(Val) }
            "foo": Definition { pos: 9..12, kind: Local(Val) }
          }
        }
      ]
    "#],
  );
}
