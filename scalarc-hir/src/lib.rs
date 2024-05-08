use std::{collections::HashMap, sync::Arc};

use la_arena::RawIdx;
use scalarc_source::{FileId, SourceDatabase, TargetId};
use scalarc_syntax::{
  ast::{AstNode, Item},
  SyntaxNodePtr, TextRange, TextSize,
};
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
  pub pos:   FileRange,
  pub name:  Name,
  pub scope: ScopeId,
  pub kind:  DefinitionKind,

  /// This is a pointer to the definition node.
  pub node: SyntaxNodePtr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference {
  pub pos: FileRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefinitionKind {
  Local(LocalDefinition),
  Global(GlobalDefinition),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LocalDefinition {
  Val(Option<Type>),
  Var,
  Parameter,
  Def(Signature),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GlobalDefinition {
  Class,
  Object,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
  pub elems: Vec<Name>,
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: SourceDatabase {
  #[salsa::invoke(ast::item_id_map)]
  fn item_id_map(&self, file: FileId) -> Arc<ast::ItemIdMap>;

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

// Each file in scala declares its entire package at the top of the file. So the
// resulting definition map for each file can be entirely determined just from
// parsing a file. The path of the file doesn't affect the result of this
// query.
fn definitions_for_file(db: &dyn HirDatabase, file: FileId) -> DefinitionMap {
  let ast = db.parse(file);

  // TODO: Parse `package` statements out of `ast`.

  let items = ast
    .tree()
    .items()
    .filter_map(|item| match item {
      Item::ObjectDef(c) => {
        let name = c.id_token()?;

        Some((
          Path { elems: vec![name.text().into()] },
          Definition {
            pos:   FileRange { file, range: name.text_range() },
            name:  name.text().into(),
            scope: ScopeId::from_raw(RawIdx::from_u32(0)), // FIXME
            kind:  DefinitionKind::Global(GlobalDefinition::Object),

            node: SyntaxNodePtr::new(c.syntax()),
          },
        ))
      }
      Item::ClassDef(c) => {
        let name = c.id_token()?;

        Some((
          Path { elems: vec![name.text().into()] },
          Definition {
            pos:   FileRange { file, range: name.text_range() },
            name:  name.text().into(),
            scope: ScopeId::from_raw(RawIdx::from_u32(0)), // FIXME
            kind:  DefinitionKind::Global(GlobalDefinition::Class),

            node: SyntaxNodePtr::new(c.syntax()),
          },
        ))
      }
      _ => None,
    })
    .collect();

  DefinitionMap { items }
}

impl Path {
  pub fn new() -> Self { Path { elems: vec![] } }
}
