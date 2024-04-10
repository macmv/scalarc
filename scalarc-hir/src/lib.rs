use std::collections::HashMap;

use scalarc_source::{FileId, SourceDatabase, TargetId};
use scalarc_syntax::ast::Item;
use tree::Name;

#[cfg(test)]
mod tests;

#[macro_use]
extern crate log;

pub mod tree;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionMap {
  pub items: HashMap<Path, ()>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
  pub elems: Vec<Name>,
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: SourceDatabase {
  fn definitions_for_target(&self, target: TargetId) -> DefinitionMap;

  fn definitions_for_file(&self, file: FileId) -> DefinitionMap;
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
        let name = c.id_token()?.text().into();

        Some((Path { elems: vec![name] }, ()))
      }
      Item::ClassDef(c) => {
        let name = c.id_token()?.text().into();

        Some((Path { elems: vec![name] }, ()))
      }
      _ => None,
    })
    .collect();

  DefinitionMap { items }
}
