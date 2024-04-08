//! A virtual filesystem that tracks all the changes from the LSP client.

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

use scalarc_source::FileId;

pub struct Files {
  files:       HashMap<PathBuf, String>,
  ids:         HashMap<PathBuf, FileId>,
  reverse_ids: HashMap<FileId, PathBuf>,
  changes:     Vec<FileId>,

  pub workspace: PathBuf,
}

impl Files {
  pub fn new(workspace: PathBuf) -> Self {
    Files {
      files: HashMap::new(),
      ids: HashMap::new(),
      reverse_ids: HashMap::new(),
      changes: vec![],
      workspace,
    }
  }

  pub fn canonicalize(&self, path: &Path) -> Option<PathBuf> {
    let path = path.to_path_buf();
    if path.is_absolute() {
      path.strip_prefix(&self.workspace).ok().map(|p| p.to_path_buf())
    } else {
      Some(path)
    }
  }

  pub fn read(&self, id: FileId) -> String {
    let path = self.reverse_ids.get(&id).unwrap();

    let Some(path) = self.canonicalize(path) else { return "".into() };
    self.files.get(&path).cloned().unwrap_or_default()
  }
  pub fn write(&mut self, id: FileId, contents: String) {
    let path = self.reverse_ids.get(&id).unwrap();

    let Some(path) = self.canonicalize(path) else { return };
    self.intern_path(&path);
    self.files.insert(path.clone(), contents);
    self.changes.push(id);
  }

  pub fn take_changes(&mut self) -> Vec<FileId> { self.changes.drain(..).collect() }

  pub fn create(&mut self, path: &Path) -> FileId {
    let path = self.canonicalize(path).unwrap();
    self.intern_path(&path);
    self.ids[&path]
  }

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

  pub fn id_to_path(&self, file_id: FileId) -> &Path {
    match self.reverse_ids.get(&file_id) {
      Some(id) => id,
      None => panic!("no such file with id {file_id:?}"),
    }
  }

  fn intern_path(&mut self, path: &Path) {
    if !self.ids.contains_key(path) {
      let id = FileId::new_raw(self.ids.len() as u32);
      self.ids.insert(path.into(), id);
      self.reverse_ids.insert(id, path.into());
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn path_to_id() {
    let mut files = Files::new(PathBuf::new());

    let id = files.create(Path::new("foo"));
    files.write(id, "bar".to_string());

    let id = files.path_to_id(Path::new("foo"));
    assert_eq!(id, FileId::new_raw(0));
  }
}
