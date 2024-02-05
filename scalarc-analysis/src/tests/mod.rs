use scalarc_source::{FileId, SourceDatabase};
use scalarc_syntax::TextSize;

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
