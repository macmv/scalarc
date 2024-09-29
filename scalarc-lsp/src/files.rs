//! A virtual filesystem that tracks all the changes from the LSP client.

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

use scalarc_source::{FileId, SourceRootId};

pub struct Files {
  files:       HashMap<FileId, File>,
  file_lookup: HashMap<FilePath, FileId>,

  roots:       HashMap<SourceRootId, SourceRoot>,
  root_lookup: HashMap<PathBuf, SourceRootId>,

  changes: Vec<FileId>,

  pub workspace: PathBuf,
}

struct File {
  contents: String,
  path:     FilePath,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum FilePath {
  Rooted { root: SourceRootId, relative_path: PathBuf },
  // Some files don't have a source root, in which case we just leave this blank.
  Absolute(PathBuf),
}

struct SourceRoot {
  absolute_path: PathBuf,
}

impl Files {
  pub fn new(workspace: PathBuf) -> Self {
    Files {
      files: HashMap::new(),
      file_lookup: HashMap::new(),
      roots: HashMap::new(),
      root_lookup: HashMap::new(),
      changes: vec![],
      workspace,
    }
  }

  fn make_file_path(&self, root_id: SourceRootId, path: &Path) -> FilePath {
    assert!(path.is_absolute(), "cannot create source root for relative path {}", path.display());

    let root = self.roots.get(&root_id).unwrap();

    if let Ok(rel) = path.strip_prefix(&root.absolute_path) {
      FilePath::Rooted { root: root_id, relative_path: rel.to_path_buf() }
    } else {
      FilePath::Absolute(path.to_path_buf())
    }
  }

  pub fn read(&self, id: FileId) -> String {
    let file = self.files.get(&id).unwrap();
    file.contents.clone()
  }
  pub fn write(&mut self, id: FileId, contents: String) {
    self.files.get_mut(&id).unwrap().contents = contents;
    self.changes.push(id);
  }

  pub fn take_changes(&mut self) -> Vec<FileId> { self.changes.drain(..).collect() }

  pub fn create_source_root(&mut self, root: SourceRootId, path: &Path) {
    assert!(path.is_absolute(), "cannot create source root for relative path {}", path.display());

    self.roots.insert(root, SourceRoot { absolute_path: path.to_path_buf() });
    self.root_lookup.insert(path.into(), root);
  }

  #[track_caller]
  pub fn create(&mut self, path: &Path) -> FileId {
    let root = self.source_root_for_path(path);

    let path = match root {
      Some(r) => self.make_file_path(r, path),
      None => FilePath::Absolute(path.to_path_buf()),
    };
    let id = FileId::new_raw(self.files.len() as u32);

    self.file_lookup.insert(path.clone(), id);
    self.files.insert(id, File { contents: String::new(), path });

    id
  }

  #[track_caller]
  pub fn get_relative(&self, root: SourceRootId, path: &Path) -> Option<FileId> {
    assert!(path.is_relative(), "cannot find source root for absolute path {}", path.display());

    self.file_lookup.get(&FilePath::Rooted { root, relative_path: path.to_path_buf() }).copied()
  }

  #[track_caller]
  pub fn get_absolute(&self, path: &Path) -> Option<FileId> {
    assert!(path.is_absolute(), "cannot lookup absolute for relative path {}", path.display());

    match self.source_root_for_path(path) {
      Some(root) => {
        let relative = path.strip_prefix(&self.roots[&root].absolute_path).unwrap();

        self
          .file_lookup
          .get(&FilePath::Rooted { root, relative_path: relative.to_path_buf() })
          .copied()
      }
      None => self.file_lookup.get(&FilePath::Absolute(path.to_path_buf())).copied(),
    }
  }

  #[track_caller]
  fn source_root_for_path(&self, path: &Path) -> Option<SourceRootId> {
    assert!(path.is_absolute(), "cannot find source root for relative path {}", path.display());

    let mut p = path.to_path_buf();
    while p.pop() {
      if let Some(id) = self.root_lookup.get(&p) {
        return Some(*id);
      }
    }

    None
  }

  pub fn id_to_absolute_path(&self, id: FileId) -> PathBuf {
    let file = self.files.get(&id).unwrap();
    match &file.path {
      FilePath::Rooted { root, relative_path } => {
        let root = self.roots.get(root).unwrap();
        root.absolute_path.join(relative_path)
      }
      FilePath::Absolute(path) => path.clone(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn get_works() {
    let mut files = Files::new(PathBuf::new());
    let root = SourceRootId::from_raw(0.into());
    let file = FileId::new_raw(0);

    files.create_source_root(root, Path::new("/foo"));

    let id = files.create(Path::new("/foo/bar"));
    files.write(id, "bar".to_string());

    let id = files.get_absolute(Path::new("/foo/bar"));
    assert_eq!(id, Some(file));

    let id = files.get_relative(root, Path::new("bar"));
    assert_eq!(id, Some(file));
  }

  #[test]
  fn get_works_with_no_root() {
    let mut files = Files::new(PathBuf::new());
    let file = FileId::new_raw(0);

    let id = files.create(Path::new("/foo/bar"));
    files.write(id, "bar".to_string());

    let id = files.get_absolute(Path::new("/foo/bar"));
    assert_eq!(id, Some(file));
  }
}
