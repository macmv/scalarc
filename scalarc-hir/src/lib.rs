#[cfg(test)]
mod tests;

pub mod body;
pub mod lower;
pub mod tree;

mod source_map;

use std::{
  hash::{Hash, Hasher},
  sync::Arc,
};

use la_arena::Idx;
use scalarc_source::{FileId, SourceDatabase, TargetId};
use tree::Package;
use url::Url;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: InternDatabase {
  #[salsa::invoke(tree::workspace_map)]
  fn workspace_map(&self) -> Vec<Arc<Package>>;

  #[salsa::invoke(tree::file_package)]
  fn file_package(&self, file_id: FileId) -> Arc<Package>;

  #[salsa::invoke(source_map::source_map)]
  fn source_map(&self, file_id: FileId) -> Arc<source_map::SourceMap>;

  #[salsa::invoke(source_map::span_map)]
  fn span_map(&self, file_id: FileId) -> Arc<source_map::SpanMap>;
}

#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase: SourceDatabase {
  #[salsa::interned]
  fn intern_def(&self, def: DefLoc) -> DefId;
  #[salsa::interned]
  fn intern_val(&self, def: ValLoc) -> ValId;
}

macro_rules! intern_key {
  ($name:ident, $loc:ident, $lookup:ident) => {
    impl salsa::InternKey for $name {
      fn from_intern_id(v: salsa::InternId) -> Self { Self(v) }
      fn as_intern_id(&self) -> salsa::InternId { self.0 }
    }

    impl $name {
      pub fn lookup(self, db: &dyn InternDatabase) -> $loc { db.$lookup(self) }
    }
  };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(salsa::InternId);
type DefLoc = ItemLoc<tree::Def>;
intern_key!(DefId, DefLoc, lookup_intern_def);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValId(salsa::InternId);
type ValLoc = ItemLoc<tree::Val>;
intern_key!(ValId, ValLoc, lookup_intern_val);

pub trait ItemTreeNode {}

#[derive(Debug)]
pub struct ItemLoc<N: ItemTreeNode> {
  pub container: FileId,
  pub id:        Idx<N>,
}

impl<N: ItemTreeNode> Clone for ItemLoc<N> {
  fn clone(&self) -> Self { Self { container: self.container, id: self.id } }
}

impl<N: ItemTreeNode> Copy for ItemLoc<N> {}

impl<N: ItemTreeNode> PartialEq for ItemLoc<N> {
  fn eq(&self, other: &Self) -> bool { self.container == other.container && self.id == other.id }
}

impl<N: ItemTreeNode> Eq for ItemLoc<N> {}

impl<N: ItemTreeNode> Hash for ItemLoc<N> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.container.hash(state);
    self.id.hash(state);
  }
}

pub struct Target {
  id: TargetId,
}

impl Target {
  pub fn bsp_id(&self, db: &dyn HirDatabase) -> Url {
    db.workspace().targets[self.id].bsp_id.clone()
  }

  pub fn dependencies(&self, db: &dyn HirDatabase) -> Vec<Target> {
    db.workspace().targets[self.id].dependencies.iter().map(|id| Target { id: *id }).collect()
  }
}
