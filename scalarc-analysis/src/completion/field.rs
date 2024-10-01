use super::{Completer, Completion, CompletionKind};
use scalarc_hir::{DefinitionKey, GlobalDefinition, GlobalDefinitionKind, HirDatabase, Type};
use scalarc_source::{SourceDatabase, TargetId};
use std::collections::HashSet;

impl Completer<'_> {
  pub fn field_completions(&self, target: TargetId, ty: Type) -> Vec<Completion> {
    let key = match ty {
      Type::Object(ref path) => DefinitionKey::Object(path.clone()),
      Type::Instance(ref path) => DefinitionKey::Instance(path.clone()),
      _ => return vec![],
    };

    for target in self.db.workspace().all_dependencies(target) {
      let defs = self.db.definitions_for_target(target);

      if let Some(def) = defs.items.get(&key) {
        return self.fields_of_def(def).unwrap_or_default();
      }
    }

    vec![]
  }

  fn fields_of_def(&self, def: &GlobalDefinition) -> Option<Vec<Completion>> {
    let body = match def.kind {
      GlobalDefinitionKind::Class(Some(body_id)) => body_id,
      GlobalDefinitionKind::Object(Some(body_id)) => body_id,
      _ => return None,
    };

    let scopes = self.db.scopes_of(def.file_id);
    let scope = scopes.get(body)?;

    let mut completions = vec![];
    let mut names = HashSet::new();
    for (_, def) in &scope.declarations {
      if names.insert(def.name.clone()) {
        completions.push(Completion {
          label: def.name.as_str().into(),
          kind:  CompletionKind::Global(def.kind.clone()),
        });
      }
    }

    Some(completions)
  }
}
