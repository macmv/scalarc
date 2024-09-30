use super::{Completer, Completion};
use scalarc_hir::{DefinitionKey, DefinitionKind, FileLocation, HirDatabase};
use scalarc_source::SourceDatabase;
use std::collections::HashSet;

impl Completer<'_> {
  pub fn top_level_completions(&self, pos: FileLocation) -> Vec<Completion> {
    let Some(source_root) = self.db.file_source_root(pos.file) else { return vec![] };
    let target = self.db.source_root_target(source_root);

    let mut definitions = vec![];
    for target in self.db.workspace().all_dependencies(target) {
      definitions.extend(self.db.definitions_for_target(target).items.into_iter().filter(
        |(_, def)| match def.kind {
          DefinitionKind::Object(_) => true,
          _ => false,
        },
      ));
    }

    let mut completions = definitions
      .into_iter()
      .map(|(key, def)| Completion {
        label: match key {
          DefinitionKey::Instance(_) => unreachable!(),
          DefinitionKey::Object(mut p) => p.elems.pop().unwrap().into_string(),
        },
        kind:  def.kind,
      })
      .collect::<Vec<_>>();

    let definitions = self.db.defs_at_index(pos.file, pos.index);

    let mut names = HashSet::new();
    for def in definitions {
      match def.kind {
        DefinitionKind::Val(_) | DefinitionKind::Def(_) | DefinitionKind::Parameter => {
          if names.insert(def.name.clone()) {
            completions.push(Completion { label: def.name.as_str().into(), kind: def.kind });
          }
        }
        _ => continue,
      }
    }

    completions
  }
}
