use std::{collections::HashMap, sync::Arc};

use ast::ErasedAstId;
use scalarc_source::{FileId, SourceDatabase, TargetId};
use scalarc_syntax::{ast::BlockExpr, TextRange, TextSize};
use scope::{FileScopes, ScopeId};
use tree::Name;

#[cfg(test)]
mod tests;

#[macro_use]
extern crate log;

pub mod analysis;
mod ast;
pub mod scope;
pub mod tree;
pub mod types;

pub use types::{Params, Signature, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionMap {
  pub items: HashMap<Path, Definition>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
  pub parent_scope: ScopeId,
  pub item_id:      ErasedAstId,
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
  Class,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
  pub elems: Vec<Name>,
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: SourceDatabase {
  #[salsa::invoke(ast::item_id_map)]
  fn item_id_map(&self, file: FileId) -> Arc<ast::AstIdMap>;

  fn definitions_for_target(&self, target: TargetId) -> DefinitionMap;

  fn definitions_for_file(&self, file: FileId) -> DefinitionMap;

  #[salsa::invoke(tree::ast_for_file)]
  fn hir_ast(&self, file: FileId) -> tree::Ast;

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

  #[salsa::invoke(ast::hir_ast_for_scope)]
  fn hir_ast_for_scope(
    &self,
    file: FileId,
    scope: Option<ast::AstId<BlockExpr>>,
  ) -> Arc<ast::Block>;

  #[salsa::invoke(types::type_of_expr)]
  fn type_of_expr(
    &self,
    file_id: FileId,
    scope: Option<ast::AstId<BlockExpr>>,
    expr: ast::ExprId,
  ) -> Option<Type>;
}

fn definitions_for_target(db: &dyn HirDatabase, target: TargetId) -> DefinitionMap {
  let mut items = HashMap::new();

  let workspace = db.workspace();
  for &root in workspace.targets[target].source_roots.iter() {
    for &file in workspace.source_roots[root].sources.iter() {
      let definitions = db.definitions_for_file(file);
      items.extend(definitions.items);
    }
  }

  DefinitionMap { items }
}

// FIXME: Replace with the zero scope for this file.
fn definitions_for_file(db: &dyn HirDatabase, file: FileId) -> DefinitionMap {
  DefinitionMap { items: HashMap::new() }
}

impl Path {
  pub fn new() -> Self { Path { elems: vec![] } }
}
