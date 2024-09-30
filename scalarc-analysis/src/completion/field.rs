use super::{Completion, CompletionsDatabase};
use scalarc_hir::{Definition, DefinitionKey, DefinitionKind, Type};
use scalarc_source::FileId;
use std::collections::HashSet;

pub fn field_completions(
  db: &dyn CompletionsDatabase,
  file_id: FileId,
  ty: Type,
) -> Option<Vec<Completion>> {
  let key = match ty {
    Type::Object(ref path) => DefinitionKey::Object(path.clone()),
    Type::Instance(ref path) => DefinitionKey::Class(path.clone()),
    _ => return None,
  };

  let mut targets = vec![db.file_target(file_id)?];

  while let Some(target) = targets.pop() {
    let defs = db.definitions_for_target(target);

    if let Some(def) = defs.items.get(&key) {
      return fields_of_def(db, def);
    }

    for dep in db.workspace().targets[target].dependencies.iter() {
      targets.push(*dep);
    }
  }

  None
}

fn fields_of_def(db: &dyn CompletionsDatabase, def: &Definition) -> Option<Vec<Completion>> {
  let body = match def.kind {
    DefinitionKind::Class(Some(body_id)) => body_id,
    DefinitionKind::Object(Some(body_id)) => body_id,
    _ => return None,
  };

  let scopes = db.scopes_of(def.file_id);
  let scope = scopes.get(body)?;

  let mut completions = vec![];
  let mut names = HashSet::new();
  for (_, def) in &scope.declarations {
    if names.insert(def.name.clone()) {
      completions.push(Completion { label: def.name.as_str().into(), kind: def.kind.clone() });
    }
  }

  Some(completions)
}
