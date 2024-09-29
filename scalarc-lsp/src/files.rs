//! A virtual filesystem that tracks all the changes from the LSP client.

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

use scalarc_source::{FileId, SourceRootId};

pub struct Files {
  files:       HashMap<PathBuf, String>,
  ids:         HashMap<PathBuf, FileId>,
  reverse_ids: HashMap<FileId, PathBuf>,

  file_roots: HashMap<FileId, SourceRootId>,
  root_paths: HashMap<PathBuf, SourceRootId>,
  roots:      HashMap<SourceRootId, PathBuf>,

  changes: Vec<FileId>,

  pub workspace: PathBuf,
}

impl Files {
  pub fn new(workspace: PathBuf) -> Self {
    Files {
      files: HashMap::new(),
      ids: HashMap::new(),
      reverse_ids: HashMap::new(),
      file_roots: HashMap::new(),
      root_paths: HashMap::new(),
      roots: HashMap::new(),
      changes: vec![],
      workspace,
    }
  }

  pub fn canonicalize(&self, root: SourceRootId, path: &Path) -> Option<PathBuf> {
    let path = path.to_path_buf();
    if path.is_absolute() {
      path.strip_prefix(&self.workspace).ok().map(|p| p.to_path_buf())
    } else {
      Some(path)
    }
  }

  pub fn read(&self, id: FileId) -> String {
    let path = self.reverse_ids.get(&id).unwrap();
    let root = *self.file_roots.get(&id).unwrap();

    let Some(path) = self.canonicalize(root, path) else { return "".into() };
    self.files.get(&path).cloned().unwrap_or_default()
  }
  pub fn write(&mut self, id: FileId, contents: String) {
    let path = self.reverse_ids.get(&id).unwrap();
    let root = *self.file_roots.get(&id).unwrap();

    let Some(path) = self.canonicalize(root, path) else { return };
    self.intern_path(root, &path);
    self.files.insert(path.clone(), contents);
    self.changes.push(id);
  }

  pub fn take_changes(&mut self) -> Vec<FileId> { self.changes.drain(..).collect() }

  pub fn create_source_root(&mut self, root: SourceRootId, path: &Path) {
    assert!(path.is_absolute(), "cannot create source root for relative path {}", path.display());

    self.root_paths.insert(path.into(), root);
    self.roots.insert(root, path.into());
  }

  #[track_caller]
  pub fn create(&mut self, path: &Path) -> FileId {
    let root = self.source_root_for_path(path);

    let path = self.canonicalize(root, path).unwrap();
    self.intern_path(root, &path);
    self.ids[&path]
  }

  pub fn get(&self, root: SourceRootId, path: &Path) -> Option<FileId> {
    let path = self.canonicalize(root, path).unwrap();
    self.ids.get(&path).copied()
  }

  pub fn path_to_id(&self, path: &Path) -> FileId {
    let root = self.source_root_for_path(path);

    let Some(path) = self.canonicalize(root, path) else {
      panic!("path {} not in workspace {}", path.display(), self.workspace.display())
    };

    match self.ids.get(&path) {
      Some(id) => *id,
      None => {
        info!("{:?}", &self.files.keys());
        panic!("no such file at {}", path.display())
      }
    }
  }

  #[track_caller]
  fn source_root_for_path(&self, path: &Path) -> SourceRootId {
    assert!(path.is_absolute(), "cannot find source root for relative path {}", path.display());

    let mut p = path.to_path_buf();
    while p.pop() {
      if let Some(id) = self.root_paths.get(&p) {
        return *id;
      }
    }
    panic!("no source root for path {}", path.display())
  }

  pub fn id_to_path(&self, file_id: FileId) -> &Path {
    match self.reverse_ids.get(&file_id) {
      Some(id) => id,
      None => panic!("no such file with id {file_id:?}"),
    }
  }

  fn intern_path(&mut self, root: SourceRootId, path: &Path) {
    if !self.ids.contains_key(path) {
      let id = FileId::new_raw(self.ids.len() as u32);
      self.ids.insert(path.into(), id);
      self.reverse_ids.insert(id, path.into());
      self.file_roots.insert(id, root);
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn path_to_id() {
    let mut files = Files::new(PathBuf::new());

    files.create_source_root(SourceRootId::from_raw(0.into()), Path::new("/foo"));

    let id = files.create(Path::new("/foo/bar"));
    files.write(id, "bar".to_string());

    let id = files.path_to_id(Path::new("/foo/bar"));
    assert_eq!(id, FileId::new_raw(0));
  }
}
