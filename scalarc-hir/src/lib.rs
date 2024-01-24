#[cfg(test)]
mod tests;

pub mod lower;
pub mod tree;

mod source_map;

use std::{
  hash::{Hash, Hasher},
  sync::Arc,
};

use la_arena::Idx;
use scalarc_source::{FileId, SourceDatabase};
use tree::Package;

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
  ($name:ident) => {
    impl salsa::InternKey for $name {
      fn from_intern_id(v: salsa::InternId) -> Self { Self(v) }
      fn as_intern_id(&self) -> salsa::InternId { self.0 }
    }
  };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(salsa::InternId);
type DefLoc = ItemLoc<tree::Def>;
intern_key!(DefId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValId(salsa::InternId);
type ValLoc = ItemLoc<tree::Val>;
intern_key!(ValId);

pub type PackageId = u32;

pub trait ItemTreeNode {}

#[derive(Debug)]
pub struct ItemLoc<N: ItemTreeNode> {
  pub container: PackageId,
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
