use std::{path::PathBuf, sync::Arc};

use la_arena::{Arena, Idx};
use scalarc_syntax::{Parse, SourceFile};
use url::Url;

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

  /// DO NOT USE THIS! Its just for unit tests.
  pub fn new_raw(id: u32) -> Self { FileId(id) }
}

#[derive(Debug)]
pub struct Workspace {
  pub root: PathBuf,

  pub targets: Arena<TargetData>,
}

/// Targets are similar to packages, but are slightly more granular. For
/// example, one project may have a target for its main sources, and a target
/// for its test sources.
///
/// Targets can have overlapping sources.
#[derive(Debug)]
pub struct TargetData {
  pub id:           TargetID,
  pub dependencies: Vec<TargetID>,

  pub bsp_id: Url,

  /// A list of directories which contain the source files for this target.
  pub sources: Vec<PathBuf>,
}

pub type TargetID = Idx<TargetData>;

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse<SourceFile> {
  let text = db.file_text(file_id);
  SourceFile::parse(&text)
}
