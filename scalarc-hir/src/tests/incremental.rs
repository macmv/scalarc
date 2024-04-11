use la_arena::Arena;
use scalarc_source::{FileId, SourceDatabase, TargetData, Workspace};

use crate::HirDatabase;

use super::TestDB;

fn check_workspace_recomputed(initial: &str, changed: &str) {
  let mut db = TestDB::default();
  let file = FileId::temp_new();
  let mut source_roots = Arena::new();
  let source =
    source_roots.alloc(scalarc_source::SourceRoot { path: "/".into(), sources: vec![file] });
  let mut targets = Arena::new();
  let target = targets.alloc(TargetData {
    dependencies: vec![],
    bsp_id:       "file:///".try_into().unwrap(),
    source_roots: vec![source],
  });
  db.set_workspace(Workspace { root: "/".into(), targets, source_roots }.into());
  db.set_file_source_root(file, source);

  db.set_file_text(file, initial.into());

  {
    let events = db.log_executed(|| {
      db.definitions_for_target(target);
    });
    assert!(format!("{events:?}").contains("definitions_for_target"), "{events:#?}")
  }

  // After editing the file to the changed text, the definitions for that file
  // should be the same. So, the definitions for the target should not be
  // re-computed.
  db.set_file_text(file, changed.into());

  {
    let events = db.log_executed(|| {
      db.definitions_for_target(target);
    });
    assert!(!format!("{events:?}").contains("definitions_for_target"), "{events:#?}")
  }
}

#[test]
fn typing_inside_a_function_should_not_invalidate_workspace() {
  check_workspace_recomputed(
    r"
val foo = 2 * 3
",
    r"
val foo = 2314587
",
  );
}
