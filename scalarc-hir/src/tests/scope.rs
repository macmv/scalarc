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
      writeln!(f, "    range: {:?},", s.visible)?;
      writeln!(f, "    declarations: {{")?;
      for (name, def) in &s.declarations {
        write!(f, "      ")?;
        write!(f, "{:?}: {}", name, DebugDef(&def))?;
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
      write!(f, "  {}", DebugDef(&def))?;
    }
    write!(f, "]\n")
  }
}

struct DebugDefOpt<'a>(&'a Option<Definition>);
impl fmt::Display for DebugDefOpt<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.0 {
      Some(def) => write!(f, "{}", DebugDef(&def)),
      None => writeln!(f, "None"),
    }
  }
}

struct DebugDef<'a>(&'a Definition);
impl fmt::Display for DebugDef<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let def = self.0;
    write!(f, "Definition {{ ")?;
    write!(f, "pos: {:?}, ", def.pos.range)?;
    write!(f, "name: {:?}, ", def.name.as_str())?;
    write!(f, "kind: {:?} ", def.kind)?;
    writeln!(f, "}}")
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
            "foo": Definition { pos: 9..12, name: "foo", kind: Local(Val) }
            "bar": Definition { pos: 25..28, name: "bar", kind: Local(Val) }
            "baz": Definition { pos: 41..44, name: "baz", kind: Local(Val) }
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
            "foo": Definition { pos: 9..12, name: "foo", kind: Local(Val) }
            "bar": Definition { pos: 25..28, name: "bar", kind: Local(Val) }
            "baz": Definition { pos: 79..82, name: "baz", kind: Local(Val) }
          }
        }
        Scope {
          range: 21..70,
          declarations: {
            "a": Definition { pos: 43..44, name: "a", kind: Local(Val) }
            "b": Definition { pos: 59..60, name: "b", kind: Local(Val) }
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
        Definition { pos: 43..44, name: "a", kind: Local(Val) }
        Definition { pos: 25..28, name: "bar", kind: Local(Val) }
        Definition { pos: 9..12, name: "foo", kind: Local(Val) }
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
      Definition { pos: 43..44, name: "a", kind: Local(Val) }
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
      Definition { pos: 43..44, name: "a", kind: Local(Val) }
    "#],
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
          range: 0..60,
          declarations: {
            "Foo": Definition { pos: 11..14, name: "Foo", kind: Global(Class) }
          }
        }
        Scope {
          range: 23..59,
          declarations: {
            "a": Definition { pos: 15..16, name: "a", kind: Local(Parameter) }
            "b": Definition { pos: 35..36, name: "b", kind: Local(Val) }
          }
        }
      ]
    "#],
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
        Definition { pos: 11..14, name: "Foo", kind: Global(Class) }
      ]
    "#],
  );

  defs_at(
    r#"
    class Foo(a: Int) { @@ }
    "#,
    expect![@r#"
      [
        Definition { pos: 15..16, name: "a", kind: Local(Parameter) }
        Definition { pos: 11..14, name: "Foo", kind: Global(Class) }
      ]
    "#],
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
        Definition { pos: 35..36, name: "b", kind: Local(Val) }
        Definition { pos: 15..16, name: "a", kind: Local(Parameter) }
        Definition { pos: 11..14, name: "Foo", kind: Global(Class) }
      ]
    "#],
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
          range: 0..51,
          declarations: {
            "foo": Definition { pos: 9..12, name: "foo", kind: Local(Def) }
          }
        }
        Scope {
          range: 23..50,
          declarations: {
            "a": Definition { pos: 13..14, name: "a", kind: Local(Parameter) }
            "b": Definition { pos: 35..36, name: "b", kind: Local(Val) }
          }
        }
      ]
    "#],
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
        Definition { pos: 9..12, name: "foo", kind: Local(Def) }
      ]
    "#],
  );

  defs_at(
    r#"
    def foo(a: Int) = @@a
    "#,
    expect![@r#"
      [
        Definition { pos: 13..14, name: "a", kind: Local(Parameter) }
        Definition { pos: 9..12, name: "foo", kind: Local(Def) }
      ]
    "#],
  );

  defs_at(
    r#"
    def foo(a: Int) = a@@
    "#,
    expect![@r#"
      [
        Definition { pos: 13..14, name: "a", kind: Local(Parameter) }
        Definition { pos: 9..12, name: "foo", kind: Local(Def) }
      ]
    "#],
  );
}
