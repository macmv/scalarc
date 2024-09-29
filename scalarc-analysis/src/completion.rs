use std::collections::HashSet;

use scalarc_hir::{DefinitionKind, HirDatabase};
use scalarc_source::SourceDatabase;

use crate::{database::RootDatabase, FileLocation};

pub struct Completion {
  pub label: String,
  pub kind:  DefinitionKind,
}

pub fn completions(db: &RootDatabase, pos: FileLocation) -> Vec<Completion> {
  let mut completions = vec![];

  info!("finding completions...");

  let Some(source_root) = db.file_source_root(pos.file) else { return vec![] };
  let target = db.source_root_target(source_root);

  let mut definitions = vec![];
  let mut targets = vec![target];

  while let Some(target) = targets.pop() {
    info!("searching {target:?}");
    definitions.extend(db.definitions_for_target(target).items);

    for dep in &db.workspace().targets[target].dependencies {
      targets.push(*dep);
    }
  }

  for (mut path, def) in definitions {
    completions
      .push(Completion { label: path.elems.pop().unwrap().into_string(), kind: def.kind });
  }

  let definitions = db.defs_at_index(pos.file, pos.index);

  let mut names = HashSet::new();
  for def in definitions {
    if names.insert(def.name.clone()) {
      completions.push(Completion { label: def.name.as_str().into(), kind: def.kind });
    }
  }

  info!("got {} completions!", completions.len());

  completions
}
