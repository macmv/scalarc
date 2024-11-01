use hir::{
  AstId, Binding, BindingKind, BlockId, ErasedAstId, ImportId, ParamId, PatternId, ResolutionKind,
  StmtId, UnresolvedPath,
};
use scalarc_source::{FileId, SourceDatabase, TargetId};
use scalarc_syntax::{
  ast::{self, ItemBody, ObjectDef},
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

mod lower;

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

impl From<GlobalDefinition> for AnyDefinition {
  fn from(d: GlobalDefinition) -> Self { AnyDefinition::Global(d) }
}
impl From<HirDefinition> for AnyDefinition {
  fn from(d: HirDefinition) -> Self { AnyDefinition::Hir(d) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDefinition {
  pub path:    Path,
  pub file_id: FileId,
  pub ast_id:  ErasedAstId,
  pub kind:    GlobalDefinitionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirDefinition {
  pub name:     Name,
  pub block_id: InFile<BlockId>,
  pub id:       HirDefinitionId,
  pub kind:     HirDefinitionKind,
}

impl GlobalDefinition {
  pub fn name(&self) -> &Name { self.path.elems.last().unwrap() }
}

impl HirDefinition {
  pub fn new_local(binding: &Binding, block_id: InFile<BlockId>, id: HirDefinitionId) -> Self {
    HirDefinition {
      name: Name::new(binding.name.clone()),
      id,
      block_id,
      kind: match binding.kind {
        BindingKind::Val => HirDefinitionKind::Val(binding.ty.clone()),
        BindingKind::Var => HirDefinitionKind::Val(binding.ty.clone()),
        BindingKind::Def(_) => HirDefinitionKind::Def(Signature::empty()),
        BindingKind::Pattern => HirDefinitionKind::Pattern,
        BindingKind::Object(id) => HirDefinitionKind::Object(id),
      },
    }
  }

  pub fn new_param(binding: &Binding, block_id: InFile<BlockId>, id: HirDefinitionId) -> Self {
    let ty = binding.ty.clone().unwrap_or(hir::Type::Unknown);

    HirDefinition {
      name: Name::new(binding.name.clone()),
      id,
      block_id,
      kind: HirDefinitionKind::Parameter(ty),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirDefinitionId {
  Stmt(StmtId),
  Param(ParamId),
  Pattern(PatternId),
  Import(ImportId),
}

impl AnyDefinition {
  pub fn name(&self) -> &Name {
    match self {
      AnyDefinition::Global(d) => d.name(),
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
  Class(Option<AstId<ItemBody>>, ClassKind),
  Trait(Option<AstId<ItemBody>>),
  Object(Option<AstId<ItemBody>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassKind {
  Normal,
  Case,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirDefinitionKind {
  Val(Option<hir::Type>),
  Var(Option<hir::Type>),
  Parameter(hir::Type),
  Pattern,
  Def(Signature),
  Import,

  Object(AstId<ObjectDef>),
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
  fn def_for_expr(&self, block: InFile<BlockId>, expr: hir::ExprId) -> Option<AnyDefinition>;

  #[salsa::invoke(scope::references_to)]
  fn references_to(&self, file: FileId, index: TextSize) -> Vec<Reference>;

  #[salsa::invoke(types::type_at)]
  fn type_at(&self, file: FileId, index: TextSize) -> Option<Type>;

  #[salsa::invoke(types::type_at_item)]
  fn type_at_item(&self, file: FileId, id: ErasedAstId) -> Option<Type>;

  // This query is unstable, because it contains syntax pointers in the returned
  // source map result. Use `hir_ast_for_block` for a stable result.
  #[salsa::invoke(lower::hir_ast_with_source_for_block)]
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
  #[salsa::invoke(lower::block_for_node)]
  fn block_for_node(&self, block: InFile<SyntaxNodePtr>) -> InFile<BlockId>;

  // Returns the parent BlockId of the given block.
  #[salsa::invoke(hir::parent_block)]
  fn parent_block(&self, block: InFile<BlockId>) -> Option<BlockId>;

  #[salsa::invoke(hir::lookup_name_in_block)]
  fn lookup_name_in_block(&self, block: InFile<BlockId>, name: String) -> Option<AnyDefinition>;

  #[salsa::invoke(hir::resolve_path_in_block)]
  fn resolve_path_in_block(
    &self,
    block: InFile<BlockId>,
    path: UnresolvedPath,
    kind: ResolutionKind,
  ) -> Option<Path>;

  #[salsa::invoke(types::type_of_block)]
  fn type_of_block(&self, block: InFile<BlockId>) -> Option<Type>;

  #[salsa::invoke(types::type_of_expr)]
  fn type_of_expr(&self, block: InFile<BlockId>, expr: hir::ExprId) -> Option<Type>;

  #[salsa::invoke(types::infer)]
  fn infer(&self, block: InFile<BlockId>, stop_at: Option<StmtId>) -> Arc<Inference>;
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
        let path = def.path.clone();

        match def.kind {
          GlobalDefinitionKind::Class(_, _) | GlobalDefinitionKind::Trait(_) => {
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
