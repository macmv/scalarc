use super::Completion;
use crate::database::RootDatabase;
use scalarc_hir::{DefinitionKey, FileLocation, HirDatabase};
use scalarc_source::SourceDatabase;
use std::collections::HashSet;

pub fn top_level_completions(db: &RootDatabase, pos: FileLocation) -> Vec<Completion> {
  let Some(source_root) = db.file_source_root(pos.file) else { return vec![] };
  let target = db.source_root_target(source_root);

  let mut definitions = vec![];
  let mut targets = vec![target];

  while let Some(target) = targets.pop() {
    definitions.extend(db.definitions_for_target(target).items);

    for dep in &db.workspace().targets[target].dependencies {
      targets.push(*dep);
    }
  }

  let mut completions = definitions
    .into_iter()
    .map(|(key, def)| Completion {
      label: match key {
        DefinitionKey::Class(mut p) => p.elems.pop().unwrap().into_string(),
        DefinitionKey::Object(mut p) => p.elems.pop().unwrap().into_string(),
      },
      kind:  def.kind,
    })
    .collect::<Vec<_>>();

  let definitions = db.defs_at_index(pos.file, pos.index);

  let mut names = HashSet::new();
  for def in definitions {
    if names.insert(def.name.clone()) {
      completions.push(Completion { label: def.name.as_str().into(), kind: def.kind });
    }
  }

  completions
}
