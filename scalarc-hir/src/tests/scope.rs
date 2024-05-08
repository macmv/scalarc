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
  expected.assert_eq(&DebugScopes(&actual.scopes).to_string());
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

struct DebugScopes<'a>(&'a Arena<Scope>);
impl fmt::Display for DebugScopes<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", indent(format!("{:#?}", self)))
  }
}
impl fmt::Debug for DebugScopes<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.0.iter().map(|(_, s)| DebugScope(s))).finish()
  }
}

struct DebugScope<'a>(&'a Scope);
impl fmt::Debug for DebugScope<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Scope")
      .field("range", &self.0.visible)
      .field("declarations", &DebugDeclarations(&self.0.declarations))
      .finish()
  }
}

struct DebugDeclarations<'a>(&'a Vec<(String, Definition)>);
impl fmt::Debug for DebugDeclarations<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_map().entries(self.0.iter().map(|&(ref k, ref v)| (k, DebugDef(v)))).finish()
  }
}

struct DebugDefList<'a>(&'a Vec<Definition>);
impl fmt::Display for DebugDefList<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", indent(format!("{:#?}", self)))
  }
}
impl fmt::Debug for DebugDefList<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.0.iter().map(DebugDef)).finish()
  }
}

struct DebugDefOpt<'a>(&'a Option<Definition>);
impl fmt::Display for DebugDefOpt<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", indent(format!("{:#?}", self)))
  }
}
impl fmt::Debug for DebugDefOpt<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.0 {
      Some(def) => write!(f, "{:#?}", DebugDef(&def)),
      None => writeln!(f, "None"),
    }
  }
}

struct DebugDef<'a>(&'a Definition);
impl fmt::Debug for DebugDef<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Definition")
      // .field("pos", &self.0.pos.range)
      .field("name", &self.0.name.as_str())
      .field("kind", &self.0.kind)
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
          range: 0..49,
          declarations: {
            "foo": Definition {
              name: "foo",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
            "bar": Definition {
              name: "bar",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
            "baz": Definition {
              name: "baz",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
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
          range: 0..87,
          declarations: {
            "foo": Definition {
              name: "foo",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
            "bar": Definition {
              name: "bar",
              kind: Local(
                Val(
                  None,
                ),
              ),
            },
            "baz": Definition {
              name: "baz",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
          },
        },
        Scope {
          range: 21..70,
          declarations: {
            "a": Definition {
              name: "a",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
            "b": Definition {
              name: "b",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
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
          name: "b",
          kind: Local(
            Val(
              Some(
                Type(scala.Int),
              ),
            ),
          ),
        },
        Definition {
          name: "a",
          kind: Local(
            Val(
              Some(
                Type(scala.Int),
              ),
            ),
          ),
        },
        Definition {
          name: "baz",
          kind: Local(
            Val(
              Some(
                Type(scala.Int),
              ),
            ),
          ),
        },
        Definition {
          name: "bar",
          kind: Local(
            Val(
              None,
            ),
          ),
        },
        Definition {
          name: "foo",
          kind: Local(
            Val(
              Some(
                Type(scala.Int),
              ),
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
      Definition {
        name: "a",
        kind: Local(
          Val(
            Some(
              Type(scala.Int),
            ),
          ),
        ),
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
      Definition {
        name: "a",
        kind: Local(
          Val(
            Some(
              Type(scala.Int),
            ),
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
        kind: Local(
          Def(
            Signature {
              params: [
                Params {
                  implicit: false,
                  params: [
                    (
                      Name(
                        "a",
                      ),
                      Type(Int),
                    ),
                  ],
                },
              ],
              ret: None,
            },
          ),
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
          range: 0..60,
          declarations: {
            "Foo": Definition {
              name: "Foo",
              kind: Local(
                Class,
              ),
            },
          },
        },
        Scope {
          range: 23..59,
          declarations: {
            "a": Definition {
              name: "a",
              kind: Local(
                Parameter,
              ),
            },
            "b": Definition {
              name: "b",
              kind: Local(
                Val(
                  Some(
                    Type(String),
                  ),
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
          kind: Local(
            Class,
          ),
        },
      ]"#],
  );

  defs_at(
    r#"
    class Foo(a: Int) { @@ }
    "#,
    expect![@r#"
      [
        Definition {
          name: "a",
          kind: Local(
            Parameter,
          ),
        },
        Definition {
          name: "Foo",
          kind: Local(
            Class,
          ),
        },
      ]"#],
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
          kind: Local(
            Val(
              Some(
                Type(String),
              ),
            ),
          ),
        },
        Definition {
          name: "a",
          kind: Local(
            Parameter,
          ),
        },
        Definition {
          name: "Foo",
          kind: Local(
            Class,
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
          range: 0..51,
          declarations: {
            "foo": Definition {
              name: "foo",
              kind: Local(
                Def(
                  Signature {
                    params: [
                      Params {
                        implicit: false,
                        params: [
                          (
                            Name(
                              "a",
                            ),
                            Type(Int),
                          ),
                        ],
                      },
                    ],
                    ret: None,
                  },
                ),
              ),
            },
          },
        },
        Scope {
          range: 23..50,
          declarations: {
            "a": Definition {
              name: "a",
              kind: Local(
                Parameter,
              ),
            },
            "b": Definition {
              name: "b",
              kind: Local(
                Val(
                  Some(
                    Type(String),
                  ),
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
          kind: Local(
            Def(
              Signature {
                params: [
                  Params {
                    implicit: false,
                    params: [
                      (
                        Name(
                          "a",
                        ),
                        Type(Int),
                      ),
                    ],
                  },
                ],
                ret: None,
              },
            ),
          ),
        },
      ]"#],
  );

  defs_at(
    r#"
    def foo(a: Int) = @@a
    "#,
    expect![@r#"
      [
        Definition {
          name: "a",
          kind: Local(
            Parameter,
          ),
        },
        Definition {
          name: "foo",
          kind: Local(
            Def(
              Signature {
                params: [
                  Params {
                    implicit: false,
                    params: [
                      (
                        Name(
                          "a",
                        ),
                        Type(Int),
                      ),
                    ],
                  },
                ],
                ret: None,
              },
            ),
          ),
        },
      ]"#],
  );

  defs_at(
    r#"
    def foo(a: Int) = a@@
    "#,
    expect![@r#"
      [
        Definition {
          name: "a",
          kind: Local(
            Parameter,
          ),
        },
        Definition {
          name: "foo",
          kind: Local(
            Def(
              Signature {
                params: [
                  Params {
                    implicit: false,
                    params: [
                      (
                        Name(
                          "a",
                        ),
                        Type(Int),
                      ),
                    ],
                  },
                ],
                ret: None,
              },
            ),
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
          range: 0..308,
          declarations: {
            "a": Definition {
              name: "a",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
          },
        },
        Scope {
          range: 20..43,
          declarations: {
            "b": Definition {
              name: "b",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
          },
        },
        Scope {
          range: 61..84,
          declarations: {
            "c": Definition {
              name: "c",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
          },
        },
        Scope {
          range: 230..261,
          declarations: {
            "g": Definition {
              name: "g",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
          },
        },
        Scope {
          range: 101..124,
          declarations: {
            "d": Definition {
              name: "d",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
          },
        },
        Scope {
          range: 164..184,
          declarations: {
            "e": Definition {
              name: "e",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
                ),
              ),
            },
          },
        },
        Scope {
          range: 275..306,
          declarations: {
            "h": Definition {
              name: "h",
              kind: Local(
                Val(
                  Some(
                    Type(scala.Int),
                  ),
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
    val a@@ = 3
    a
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
