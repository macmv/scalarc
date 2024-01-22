//! A virtual filesystem that tracks all the changes from the LSP client.

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

pub struct Files {
  files:   HashMap<PathBuf, String>,
  changes: Vec<PathBuf>,
}

impl Files {
  pub fn new() -> Self { Files { files: HashMap::new(), changes: vec![] } }

  pub fn read(&self, path: &Path) -> String { self.files.get(path).cloned().unwrap_or_default() }
  pub fn write(&mut self, path: &Path, contents: String) {
    self.files.insert(path.into(), contents);
    self.changes.push(path.into());
  }

  pub fn take_changes(&mut self) -> Vec<PathBuf> { self.changes.drain(..).collect() }
}
