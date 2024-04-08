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

  info!("searching for definitions of target {:?}", target);

  for &source in db.workspace().targets[target].sources.iter() {
    info!("searching for definitions of source {:?}", source);
    for file in db.source_root_files(source) {
      info!("searching for definitions of file {:?}", file);
      let definitions = db.definitions_for_file(file);
      info!("got definitions of file {:?}", definitions);
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
