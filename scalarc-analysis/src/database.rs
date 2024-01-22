use std::{fmt, path::PathBuf, sync::Arc};

use scalarc_syntax::{Parse, SourceFile};

#[salsa::database(SourceDatabaseStorage)]
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

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: std::fmt::Debug {
  /// The current workspace.
  #[salsa::input]
  fn workspace(&self) -> Arc<Workspace>;

  /// Returns the current content of the file.
  #[salsa::input]
  fn file_text(&self, file_id: FileId) -> Arc<str>;

  /// Parses the file into the syntax tree.
  fn parse(&self, file_id: FileId) -> Parse<SourceFile>;
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse<SourceFile> {
  let text = db.file_text(file_id);
  SourceFile::parse(&text)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(u32);

impl FileId {
  pub fn temp_new() -> Self { FileId(0) }
}

#[derive(Debug)]
pub struct Workspace {
  // TODO: Need a BSP
  pub path: PathBuf,
}