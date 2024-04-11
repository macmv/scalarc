use la_arena::Arena;
use scalarc_source::{FileId, SourceDatabase, TargetData, Workspace};

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
  let mut source_roots = Arena::new();
  let source =
    source_roots.alloc(scalarc_source::SourceRoot { path: "/".into(), sources: vec![file] });
  let mut targets = Arena::new();
  targets.alloc(TargetData {
    dependencies: vec![],
    bsp_id:       "file:///".try_into().unwrap(),
    source_roots: vec![source],
  });
  db.set_workspace(Workspace { root: "/".into(), targets, source_roots }.into());
  db.set_file_source_root(file, source);
  db.set_file_text(file, real_src.into());

  simple_completions(&db, crate::FileLocation { file, index: cursor.into() })
}

#[test]
fn check_completions() {
  assert_eq!(
    completions_for(
      r#"
        val x = 1
        val y = 2
        val z = 3 + 4
        |
      "#,
    ),
    vec!["x", "y", "z"]
  );

  assert_eq!(
    completions_for(
      r#"
        val x = 1
        val y = 2
        |
        val z = 3 + 4
      "#,
    ),
    vec!["x", "y"]
  );

  assert_eq!(
    completions_for(
      r#"
        val x = 1
        val y = 2
        val z = |3 + 4
      "#,
    ),
    vec!["x", "y"]
  );
}
