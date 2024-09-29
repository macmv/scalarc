use std::{path::PathBuf, sync::Arc};

use la_arena::Arena;
use scalarc_source::{FileId, SourceDatabase, SourceRoot, TargetData};

use crate::{completion::completions, database::RootDatabase};

fn simple_completions(db: &RootDatabase, cursor: crate::FileLocation) -> Vec<String> {
  let completions = completions(db, cursor);
  completions.into_iter().map(|c| c.label).collect()
}

fn completions_for(src: &str) -> Vec<String> {
  let cursor = src.find('|').unwrap() as u32;
  let real_src = src[..cursor as usize].to_string() + &src[cursor as usize + 1..];

  let mut db = RootDatabase::default();
  let file = FileId::temp_new();
  db.set_file_text(file, real_src.into());

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

  simple_completions(&db, crate::FileLocation { file, index: cursor.into() })
}

#[test]
fn check_completions() {
  assert_eq!(
    completions_for(
      r#"
        object Foo {
          val x = 1
          val y = 2
          val z = 3 + 4
          |
        }
      "#,
    ),
    vec!["Foo", "Int", "z", "y", "x"]
  );

  assert_eq!(
    completions_for(
      r#"
        object Foo {
          val x = 1
          val y = 2
          |
          val z = 3 + 4
        }
      "#,
    ),
    vec!["Foo", "Int", "y", "x"]
  );

  assert_eq!(
    completions_for(
      r#"
        object Foo {
          val x = 1
          val y = 2
          val z = |3 + 4
        }
      "#,
    ),
    vec!["Foo", "Int", "y", "x"]
  );
}
