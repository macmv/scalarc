use std::{path::PathBuf, sync::Arc};

use scalarc_syntax::{Parse, SourceFile};

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

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse<SourceFile> {
  let text = db.file_text(file_id);
  SourceFile::parse(&text)
}
