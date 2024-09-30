use hir::{AstId, BlockId, ErasedAstId};
use scalarc_source::{FileId, SourceDatabase, TargetId};
use scalarc_syntax::{
  ast::{self, ItemBody},
  SyntaxNodePtr, TextRange, TextSize,
};
use scope::{FileScopes, ScopeId};
use std::{collections::HashMap, sync::Arc};

#[cfg(test)]
mod tests;

#[macro_use]
extern crate log;

mod hir;
pub mod scope;
pub mod types;

mod name;
pub use name::Name;

use types::Inference;
pub use types::{Params, Signature, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InFile<T> {
  pub file_id: FileId,
  pub id:      T,
}

pub trait InFileExt
where
  Self: Sized,
{
  fn in_file(self, file_id: FileId) -> InFile<Self>;
}

impl<T> InFileExt for T {
  fn in_file(self, file_id: FileId) -> InFile<Self> { InFile { file_id, id: self } }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionMap {
  pub items: HashMap<DefinitionKey, Definition>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefinitionKey {
  Object(Path),
  Class(Path),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileLocation {
  pub file:  FileId,
  pub index: TextSize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileRange {
  pub file:  FileId,
  pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
  pub name:         Name,
  pub file_id:      FileId,
  pub parent_scope: ScopeId,
  pub ast_id:       ErasedAstId,
  pub kind:         DefinitionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference {
  pub pos: FileRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefinitionKind {
  Val(Option<Type>),
  Var,
  Parameter,
  Def(Signature),
  Class(Option<AstId<ItemBody>>),
  Trait(Option<AstId<ItemBody>>),
  Object(Option<AstId<ItemBody>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
  pub elems: Vec<Name>,
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: SourceDatabase {
  #[salsa::invoke(hir::ast_id_map)]
  fn ast_id_map(&self, file: FileId) -> Arc<hir::AstIdMap>;

  fn definitions_for_target(&self, target: TargetId) -> DefinitionMap;
  fn definitions_for_file(&self, file: FileId) -> DefinitionMap;

  #[salsa::invoke(scope::scopes_of)]
  fn scopes_of(&self, file: FileId) -> FileScopes;
  #[salsa::invoke(scope::def_at_index)]
  fn def_at_index(&self, file: FileId, index: TextSize) -> Option<Definition>;
  #[salsa::invoke(scope::defs_at_index)]
  fn defs_at_index(&self, file: FileId, index: TextSize) -> Vec<Definition>;
  #[salsa::invoke(scope::references_to)]
  fn references_to(&self, file: FileId, index: TextSize) -> Vec<Reference>;

  #[salsa::invoke(types::type_at)]
  fn type_at(&self, file: FileId, index: TextSize) -> Option<Type>;

  #[salsa::invoke(types::type_at_item)]
  fn type_at_item(&self, file: FileId, id: ErasedAstId) -> Option<Type>;

  // This query is unstable, because it contains syntax pointers in the returned
  // source map result. Use `hir_ast_for_scope` for a stable result.
  #[salsa::invoke(hir::hir_ast_with_source_for_scope)]
  fn hir_ast_with_source_for_scope(
    &self,
    block: InFile<BlockId>,
  ) -> (Arc<hir::Block>, Arc<hir::BlockSourceMap>);

  // This query is stable across reparses.
  fn hir_ast_for_scope(&self, block: InFile<BlockId>) -> Arc<hir::Block>;

  // This query returns a stable result across reparses, but depends on the CST
  // directly.
  fn package_for_file(&self, file: FileId) -> Option<Path>;

  // Returns the BlockId of any node in a file. This returns an `InFile` for ease
  // of use, the `file_id` will always be the same as the given file_id.
  #[salsa::invoke(hir::block_for_node)]
  fn block_for_node(&self, block: InFile<SyntaxNodePtr>) -> InFile<BlockId>;

  #[salsa::invoke(types::type_of_block)]
  fn type_of_block(&self, block: InFile<BlockId>) -> Option<Type>;

  #[salsa::invoke(types::type_of_expr)]
  fn type_of_expr(&self, block: InFile<BlockId>, expr: hir::ExprId) -> Option<Type>;

  #[salsa::invoke(types::infer)]
  fn infer(&self, block: InFile<BlockId>) -> Arc<Inference>;
}

fn hir_ast_for_scope(db: &dyn HirDatabase, block: InFile<BlockId>) -> Arc<hir::Block> {
  db.hir_ast_with_source_for_scope(block).0
}

fn definitions_for_target(db: &dyn HirDatabase, target: TargetId) -> DefinitionMap {
  let mut items = HashMap::new();

  let workspace = db.workspace();
  for &root in workspace.targets[target].source_roots.iter() {
    for &file in workspace.source_roots[root].sources.iter() {
      items.extend(db.definitions_for_file(file).items);
    }
  }

  DefinitionMap { items }
}

fn definitions_for_file(db: &dyn HirDatabase, file_id: FileId) -> DefinitionMap {
  let file_scopes = db.scopes_of(file_id);
  let package = db.package_for_file(file_id).unwrap_or_default();

  // Only the outermost scope is visible.
  //
  // FIXME: This is just wrong.
  let Some(scope) = file_scopes.scopes.iter().next() else {
    return DefinitionMap { items: HashMap::new() };
  };

  DefinitionMap {
    items: scope
      .1
      .declarations
      .iter()
      .rev()
      .map(|(_, def)| {
        let mut path = package.clone();
        path.elems.push(def.name.clone());

        match def.kind {
          DefinitionKind::Class(_) => (DefinitionKey::Class(path), def.clone()),
          DefinitionKind::Object(_) => (DefinitionKey::Object(path), def.clone()),
          _ => (DefinitionKey::Object(path), def.clone()),
        }
      })
      .collect(),
  }
}

fn package_for_file(db: &dyn HirDatabase, file: FileId) -> Option<Path> {
  let ast = db.parse(file);

  for item in ast.tree().items() {
    match item {
      ast::Item::Package(p) => {
        let mut elems = vec![];

        if let Some(p) = p.path() {
          for id in p.ids() {
            elems.push(Name::new(id.to_string()));
          }
        }

        return Some(Path { elems });
      }
      _ => {}
    }
  }

  None
}

impl Default for Path {
  fn default() -> Self { Self::new() }
}

impl Path {
  pub fn new() -> Self { Path { elems: vec![] } }
}
