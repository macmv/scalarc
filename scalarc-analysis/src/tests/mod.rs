use crate::{completion, database::RootDatabase};
use la_arena::Arena;
use salsa::ParallelDatabase;
use scalarc_source::{FileId, SourceDatabase, SourceRoot, TargetData};
use scalarc_test::{expect, Expect};
use std::{path::PathBuf, sync::Arc};

mod goto_definition;

fn simple_completions(db: &RootDatabase, cursor: crate::FileLocation) -> Vec<String> {
  let completions = completion::completions(db, cursor);
  completions.into_iter().map(|c| c.label).collect()
}

fn setup_db(src: &str) -> RootDatabase {
  let mut db = RootDatabase::default();
  let file = FileId::temp_new();
  db.set_file_text(file, src.into());

  let std = FileId::new_raw(1);
  db.set_file_text(std, include_str!("../../../scalarc-hir/src/tests/ministd.scala").into());

  // Build a workspace with two targets: one for std, and one for this test.
  let mut targets = Arena::new();
  let mut source_roots = Arena::new();

  let std_source = source_roots.alloc(SourceRoot { path: PathBuf::new(), sources: vec![std] });
  let test_source = source_roots.alloc(SourceRoot { path: PathBuf::new(), sources: vec![file] });

  let std_target = targets.alloc(TargetData {
    dependencies: vec![],
    bsp_id:       "file:///std".parse().unwrap(),
    source_roots: vec![std_source],
  });

  targets.alloc(TargetData {
    dependencies: vec![std_target],
    bsp_id:       "file:///test".parse().unwrap(),
    source_roots: vec![test_source],
  });

  db.set_file_source_root(std, Some(std_source));
  db.set_file_source_root(file, Some(test_source));

  let workspace = scalarc_source::Workspace { root: Default::default(), targets, source_roots };
  db.set_workspace(Arc::new(workspace));

  db
}

fn completions_for(src: &str, expect: Expect) {
  let cursor = src.find('|').unwrap() as u32;
  let real_src = src[..cursor as usize].to_string() + &src[cursor as usize + 1..];

  let file = FileId::temp_new();
  let db = setup_db(&real_src);

  let completions = simple_completions(&db, crate::FileLocation { file, index: cursor.into() });
  let expected = format!("[{}]", completions.join(", "));

  expect.assert_eq(&expected);
}

fn goto_definition(src: &str, expect: Expect) {
  let src = src
    .trim_start_matches(|c| c == '\n')
    .lines()
    .map(|l| l.strip_prefix("    ").unwrap_or(l))
    .collect::<Vec<_>>()
    .join("\n");

  let cursor = src.find('|').unwrap();
  let real_src = src[..cursor as usize].to_string() + &src[cursor as usize + 1..];

  let file = FileId::temp_new();
  let db = setup_db(&real_src);

  let analysis = crate::Analysis { db: db.snapshot() };

  let (_, range) = analysis
    .definition_for_name(crate::FileLocation { file, index: (cursor as u32).into() })
    .expect("cancelled?")
    .expect("no definition found");

  let actual_start = u32::from(range.range.start()) as usize;
  let actual_end = u32::from(range.range.end()) as usize;

  let expected = format!(
    "{}@{}@{}",
    &real_src[..actual_start],
    &real_src[actual_start..actual_end],
    &real_src[actual_end..],
  );

  expect.assert_eq(&expected);
}

#[test]
fn check_completions() {
  completions_for(
    r#"
      object Foo {
        val x = 1
        val y = 2
        val z = 3 + 4
        |
      }
    "#,
    expect![@"[Foo, Int, x, y, z]"],
  );

  completions_for(
    r#"
      object Foo {
        val x = 1
        val y = 2
        |
        val z = 3 + 4
      }
    "#,
    expect![@"[Foo, Int, x, y]"],
  );

  completions_for(
    r#"
      object Foo {
        val x = 1
        val y = 2
        val z = |3 + 4
      }
    "#,
    expect![@"[Foo, Int, x, y, z]"],
  );
}

#[test]
fn lhs_of_dot_access() {
  completions_for(
    r#"
      object Foo {
        val x = 1
      }

      F|oo.foo
    "#,
    expect![@"[Foo, Int, Foo]"],
  );
}

#[test]
fn dot_access() {
  completions_for(
    r#"
     object Foo {
       val x = 1
     }

     Foo.|foo
   "#,
    expect![@"[x]"],
  );
}

#[test]
fn dot_access_invalid_syntax() {
  completions_for(
    r#"
     object Foo {
       val x = 1
     }

     Foo.|
   "#,
    expect![@"[x]"],
  );
}

#[test]
fn complete_def() {
  completions_for(
    r#"
     object Foo {
       def a = 3
       def b = |
     }
   "#,
    expect![@"[Foo, Int, a, b]"],
  );
}

#[test]
fn complete_params() {
  completions_for(
    r#"
     class Foo(a: Int) {
       |
     }
   "#,
    expect![@"[Int]"],
  );

  completions_for(
    r#"
     class Foo(val a: Int) {
       |
     }
   "#,
    expect![@"[Int]"],
  );
}
