use std::{collections::HashSet, path::PathBuf, sync::Arc};

use la_arena::{Arena, Idx};
use scalarc_syntax::{Parse, SourceFile};
use url::Url;

mod source_root;

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

  #[salsa::input]
  fn file_source_root(&self, file_id: FileId) -> Option<SourceRootId>;

  #[salsa::invoke(source_root::source_root_target)]
  fn source_root_target(&self, id: SourceRootId) -> TargetId;

  #[salsa::invoke(source_root::file_target)]
  fn file_target(&self, file_id: FileId) -> Option<TargetId>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(u32);

impl FileId {
  pub fn temp_new() -> Self { FileId(0) }

  /// DO NOT USE THIS! Its just for unit tests.
  pub fn new_raw(id: u32) -> Self { FileId(id) }
}

#[derive(Default, Debug)]
pub struct Workspace {
  pub root: PathBuf,

  pub targets:      Arena<TargetData>,
  pub source_roots: Arena<SourceRoot>,
}

impl Workspace {
  pub fn all_dependencies(&self, target: TargetId) -> DependencyIter {
    DependencyIter { workspace: self, seen: HashSet::new(), stack: vec![target] }
  }
}

pub struct DependencyIter<'a> {
  workspace: &'a Workspace,
  seen:      HashSet<TargetId>,
  stack:     Vec<TargetId>,
}

impl Iterator for DependencyIter<'_> {
  type Item = TargetId;

  fn next(&mut self) -> Option<Self::Item> {
    let mut target = self.stack.pop()?;
    while !self.seen.insert(target) {
      target = self.stack.pop()?;
    }
    self.stack.extend(self.workspace.targets[target].dependencies.iter().copied());
    Some(target)
  }
}

/// Targets are similar to packages, but are slightly more granular. For
/// example, one project may have a target for its main sources, and a target
/// for its test sources.
///
/// Target sources are unique to each target.
#[derive(Debug)]
pub struct TargetData {
  pub dependencies: Vec<TargetId>,

  pub bsp_id: Url,

  /// A list of directories which contain the source files for this target.
  pub source_roots: Vec<SourceRootId>,
}

pub type TargetId = Idx<TargetData>;

#[derive(Debug)]
pub struct SourceRoot {
  pub path:    PathBuf,
  pub sources: Vec<FileId>,
}

pub type SourceRootId = Idx<SourceRoot>;

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse<SourceFile> {
  let text = db.file_text(file_id);
  SourceFile::parse(&text)
}
