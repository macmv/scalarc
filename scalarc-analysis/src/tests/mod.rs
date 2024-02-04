use scalarc_source::{FileId, SourceDatabase};
use scalarc_syntax::TextSize;

use crate::{completion::completions, database::RootDatabase};

fn simple_completions(db: &RootDatabase, cursor: crate::FileLocation) -> Vec<String> {
  let completions = completions(db, cursor);
  completions.into_iter().map(|c| c.label).collect()
}

#[test]
fn check_completions() {
  let src = r#"
    val x = 1
    val y = 2
    val z = 3 + 4
  "#;

  let mut db = RootDatabase::default();
  let file = FileId::temp_new();
  db.set_file_text(file, src.into());

  let completions = simple_completions(&db, crate::FileLocation { file, index: TextSize::new(0) });
  assert!(completions.is_empty());

  let completions = simple_completions(
    &db,
    crate::FileLocation { file, index: TextSize::new(src.len() as u32 - 2) },
  );
  assert_eq!(completions, vec!["x", "y", "z"]);
}
