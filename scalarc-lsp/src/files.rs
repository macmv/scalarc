//! A virtual filesystem that tracks all the changes from the LSP client.

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

pub struct Files {
  files: HashMap<PathBuf, String>,
}

impl Files {
  pub fn new() -> Self { Files { files: HashMap::new() } }

  pub fn read(&self, path: &Path) -> String { self.files.get(path).cloned().unwrap_or_default() }
  pub fn write(&mut self, path: &Path, contents: String) {
    self.files.insert(path.into(), contents);
  }
}
