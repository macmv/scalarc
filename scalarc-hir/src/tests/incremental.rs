use scalarc_source::{FileId, SourceDatabase};

use crate::HirDatabase;

use super::TestDB;

fn check_workspace_recomputed(initial: &str, changed: &str) {
  let mut db = TestDB::default();
  let file = FileId::temp_new();
  db.set_file_text(file, initial.into());

  {
    let events = db.log_executed(|| {
      db.workspace_map();
    });
    dbg!(&events);
    assert!(format!("{events:?}").contains("workspace_map"), "{events:#?}")
  }
  db.set_file_text(file, changed.into());

  {
    let events = db.log_executed(|| {
      db.workspace_map();
    });
    dbg!(&events);
    assert!(!format!("{events:?}").contains("workspace_map"), "{events:#?}")
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
