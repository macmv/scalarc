use std::fmt;

use crate::{scope::Scope, Definition, HirDatabase};
use la_arena::Arena;
use scalarc_source::{FileId, SourceDatabase};
use scalarc_syntax::TextSize;
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

fn defs_at(src: &str, expected: Expect) {
  let cursor = src.find("@@").unwrap();
  let src = format!("{}{}", &src[..cursor], &src[cursor + 2..]);

  let db = new_db(&src);
  let actual = db.defs_at_index(FileId::temp_new(), TextSize::from(cursor as u32));
  expected.assert_eq(&DebugDefList(&actual).to_string());
}

fn def_at(src: &str, expected: Expect) {
  let cursor = src.find("@@").unwrap();
  let src = format!("{}{}", &src[..cursor], &src[cursor + 2..]);

  let db = new_db(&src);
  let actual = db.def_at_index(FileId::temp_new(), TextSize::from(cursor as u32));
  expected.assert_eq(&DebugDefOpt(&actual).to_string());
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

struct DebugDefList<'a>(&'a Vec<Definition>);
impl fmt::Display for DebugDefList<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[\n")?;
    for def in self.0.iter() {
      write!(f, "  Definition {{ ")?;
      write!(f, "pos: {:?}, ", def.pos.range)?;
      write!(f, "kind: {:?} ", def.kind)?;
      writeln!(f, "}}")?;
    }
    write!(f, "]\n")
  }
}

struct DebugDefOpt<'a>(&'a Option<Definition>);
impl fmt::Display for DebugDefOpt<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.0 {
      Some(def) => writeln!(f, "Definition {{ pos: {:?}, kind: {:?} }}", def.pos.range, def.kind),
      None => writeln!(f, "None"),
    }
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
        Definition { pos: 43..44, kind: Local(Val) }
        Definition { pos: 25..28, kind: Local(Val) }
        Definition { pos: 9..12, kind: Local(Val) }
      ]
    "#],
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
      Definition { pos: 43..44, kind: Local(Val) }
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
      Definition { pos: 43..44, kind: Local(Val) }
    "#],
  );
}
