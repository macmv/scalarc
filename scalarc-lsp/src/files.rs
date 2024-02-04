//! A virtual filesystem that tracks all the changes from the LSP client.

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

use scalarc_source::FileId;

pub struct Files {
  files:   HashMap<PathBuf, String>,
  ids:     HashMap<PathBuf, FileId>,
  changes: Vec<PathBuf>,
}

impl Files {
  pub fn new() -> Self { Files { files: HashMap::new(), ids: HashMap::new(), changes: vec![] } }

  pub fn read(&self, path: &Path) -> String { self.files.get(path).cloned().unwrap_or_default() }
  pub fn write(&mut self, path: &Path, contents: String) {
    self.intern_path(path);
    self.files.insert(path.into(), contents);
    self.changes.push(path.into());
  }

  pub fn take_changes(&mut self) -> Vec<PathBuf> { self.changes.drain(..).collect() }

  pub fn path_to_id(&self, path: &Path) -> FileId {
    match self.ids.get(path) {
      Some(id) => *id,
      None => panic!("no such file at {}", path.display()),
    }
  }

  fn intern_path(&mut self, path: &Path) {
    if !self.ids.contains_key(path) {
      self.ids.insert(path.into(), FileId::new_raw(self.ids.len() as u32));
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn path_to_id() {
    let mut files = Files::new();

    files.write(Path::new("foo"), "bar".to_string());

    let id = files.path_to_id(Path::new("foo"));
    assert_eq!(id, FileId::new_raw(0));
  }
}
