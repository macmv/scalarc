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

  pub workspace: PathBuf,
}

impl Files {
  pub fn new(workspace: PathBuf) -> Self {
    Files { files: HashMap::new(), ids: HashMap::new(), changes: vec![], workspace }
  }

  fn canonicalize(&self, path: &Path) -> Option<PathBuf> {
    let path = path.to_path_buf();
    if path.is_absolute() {
      path.strip_prefix(&self.workspace).ok().map(|p| p.to_path_buf())
    } else {
      Some(path)
    }
  }

  pub fn read(&self, path: &Path) -> String {
    let Some(path) = self.canonicalize(path) else { return "".into() };
    self.files.get(&path).cloned().unwrap_or_default()
  }
  pub fn write(&mut self, path: &Path, contents: String) {
    let Some(path) = self.canonicalize(path) else { return };
    self.intern_path(&path);
    self.files.insert(path.clone(), contents);
    self.changes.push(path);
  }

  pub fn take_changes(&mut self) -> Vec<PathBuf> { self.changes.drain(..).collect() }

  pub fn path_to_id(&self, path: &Path) -> FileId {
    let Some(path) = self.canonicalize(path) else {
      panic!("path not in workspace {}", path.display())
    };

    match self.ids.get(&path) {
      Some(id) => *id,
      None => {
        info!("{:?}", &self.files.keys());
        panic!("no such file at {}", path.display())
      }
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
    let mut files = Files::new(PathBuf::new());

    files.write(Path::new("foo"), "bar".to_string());

    let id = files.path_to_id(Path::new("foo"));
    assert_eq!(id, FileId::new_raw(0));
  }
}
