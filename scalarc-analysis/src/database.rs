use std::{fmt, sync::Arc};

use line_index::LineIndex;
use scalarc_source::FileId;

#[salsa::database(
  scalarc_source::SourceDatabaseStorage,
  scalarc_hir::HirDatabaseStorage,
  LineIndexDatabaseStorage
)]
#[derive(Default)]
pub struct RootDatabase {
  pub(crate) storage: salsa::Storage<Self>,
}
impl salsa::Database for RootDatabase {}

impl fmt::Debug for RootDatabase {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("RootDatabase").finish()
  }
}

#[salsa::query_group(LineIndexDatabaseStorage)]
pub trait LineIndexDatabase: scalarc_source::SourceDatabase {
  fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn line_index(db: &dyn LineIndexDatabase, file_id: FileId) -> Arc<LineIndex> {
  let text = db.file_text(file_id);
  Arc::new(LineIndex::new(&text))
}
