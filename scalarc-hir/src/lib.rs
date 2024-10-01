use hir::{AstId, BlockId, ErasedAstId, StmtId};
use scalarc_source::{FileId, SourceDatabase, TargetId};
use scalarc_syntax::{
  ast::{self, ItemBody},
  SyntaxNodePtr, TextRange, TextSize,
};
use scope::FileScopes;
use std::{collections::HashMap, sync::Arc};

#[cfg(test)]
mod tests;

#[macro_use]
extern crate log;

pub mod hir;
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
  pub items: HashMap<DefinitionKey, GlobalDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefinitionKey {
  // A companion object.
  Object(Path),

  // An instance of a class or trait.
  Instance(Path),
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

/// Definitions can either live in HIR (for things like local variables), or in
/// the global scope.
///
/// The difference is needed because HIR definitions are tied to a specific
/// block, while global definitions are more closely tied to files.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnyDefinition {
  Global(GlobalDefinition),
  Hir(HirDefinition),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDefinition {
  pub name:    Name,
  pub file_id: FileId,
  pub ast_id:  ErasedAstId,
  pub kind:    GlobalDefinitionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirDefinition {
  pub name:     Name,
  pub block_id: InFile<BlockId>,
  pub stmt_id:  StmtId,
  pub kind:     HirDefinitionKind,
}

impl AnyDefinition {
  pub fn name(&self) -> &Name {
    match self {
      AnyDefinition::Global(d) => &d.name,
      AnyDefinition::Hir(d) => &d.name,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference {
  pub pos: FileRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GlobalDefinitionKind {
  Class(Option<AstId<ItemBody>>),
  Trait(Option<AstId<ItemBody>>),
  Object(Option<AstId<ItemBody>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirDefinitionKind {
  Val(Option<Type>),
  Var(Option<Type>),
  Parameter,
  Def(Signature),
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
  fn definition_for_key(&self, target: TargetId, key: DefinitionKey) -> Option<GlobalDefinition>;

  #[salsa::invoke(scope::scopes_of)]
  fn scopes_of(&self, file: FileId) -> FileScopes;

  // TODO: Replace with `def_for_expr`.
  #[salsa::invoke(scope::def_at_index)]
  fn def_at_index(&self, file: FileId, index: TextSize) -> Option<AnyDefinition>;

  #[salsa::invoke(hir::def_for_expr)]
  fn def_for_expr(&self, block: InFile<BlockId>, expr: hir::ExprId) -> Option<HirDefinition>;

  #[salsa::invoke(scope::defs_at_index)]
  fn defs_at_index(&self, file: FileId, index: TextSize) -> Vec<GlobalDefinition>;
  #[salsa::invoke(scope::references_to)]
  fn references_to(&self, file: FileId, index: TextSize) -> Vec<Reference>;

  #[salsa::invoke(types::type_at)]
  fn type_at(&self, file: FileId, index: TextSize) -> Option<Type>;

  #[salsa::invoke(types::type_at_item)]
  fn type_at_item(&self, file: FileId, id: ErasedAstId) -> Option<Type>;

  // This query is unstable, because it contains syntax pointers in the returned
  // source map result. Use `hir_ast_for_block` for a stable result.
  #[salsa::invoke(hir::hir_ast_with_source_for_block)]
  fn hir_ast_with_source_for_block(
    &self,
    block: InFile<BlockId>,
  ) -> (Arc<hir::Block>, Arc<hir::BlockSourceMap>);

  // This query is stable across reparses.
  fn hir_ast_for_block(&self, block: InFile<BlockId>) -> Arc<hir::Block>;

  // This query is unstable across reparses.
  #[salsa::dependencies]
  fn hir_source_map_for_block(&self, block: InFile<BlockId>) -> Arc<hir::BlockSourceMap>;

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

fn hir_ast_for_block(db: &dyn HirDatabase, block: InFile<BlockId>) -> Arc<hir::Block> {
  db.hir_ast_with_source_for_block(block).0
}

fn hir_source_map_for_block(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
) -> Arc<hir::BlockSourceMap> {
  db.hir_ast_with_source_for_block(block).1
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
          GlobalDefinitionKind::Class(_) | GlobalDefinitionKind::Trait(_) => {
            (DefinitionKey::Instance(path), def.clone())
          }
          GlobalDefinitionKind::Object(_) => (DefinitionKey::Object(path), def.clone()),
        }
      })
      .collect(),
  }
}

fn definition_for_key(
  db: &dyn HirDatabase,
  target: TargetId,
  key: DefinitionKey,
) -> Option<GlobalDefinition> {
  for target in db.workspace().all_dependencies(target) {
    if let Some(def) = db.definitions_for_target(target).items.get(&key) {
      return Some(def.clone());
    }
  }

  None
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
