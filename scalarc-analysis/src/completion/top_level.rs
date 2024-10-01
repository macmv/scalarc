use super::{Completer, Completion, CompletionKind};
use scalarc_hir::{DefinitionKey, FileLocation, GlobalDefinitionKind, HirDatabase};
use scalarc_source::SourceDatabase;

impl Completer<'_> {
  pub fn top_level_completions(&self, pos: FileLocation) -> Vec<Completion> {
    let Some(source_root) = self.db.file_source_root(pos.file) else { return vec![] };
    let target = self.db.source_root_target(source_root);

    let mut definitions = vec![];
    for target in self.db.workspace().all_dependencies(target) {
      definitions.extend(self.db.definitions_for_target(target).items.into_iter().filter(
        |(_, def)| match def.kind {
          GlobalDefinitionKind::Object(_) => true,
          _ => false,
        },
      ));
    }

    let completions = definitions
      .into_iter()
      .map(|(key, def)| Completion {
        label: match key {
          DefinitionKey::Instance(_) => unreachable!(),
          DefinitionKey::Object(mut p) => p.elems.pop().unwrap().into_string(),
        },
        kind:  CompletionKind::Global(def.kind),
      })
      .collect::<Vec<_>>();

    completions
  }
}
