use std::collections::HashSet;

use scalarc_hir::{Definition, DefinitionKey, DefinitionKind, HirDatabase, Type};
use scalarc_parser::{SyntaxKind, T};
use scalarc_source::{FileId, SourceDatabase};
use scalarc_syntax::{ast, ast::AstNode};

use crate::{database::RootDatabase, FileLocation};

pub struct Completion {
  pub label: String,
  pub kind:  DefinitionKind,
}

pub fn completions(db: &RootDatabase, pos: FileLocation) -> Vec<Completion> {
  completions_inner(db, pos).unwrap_or_default()
}

fn completions_inner(db: &RootDatabase, pos: FileLocation) -> Option<Vec<Completion>> {
  let ast = db.parse(pos.file);
  let node = ast
    .syntax_node()
    .token_at_offset(pos.index)
    .max_by_key(|token| match token.kind() {
      T![ident] => 10,
      SyntaxKind::INT_LIT_KW => 9,

      // Whitespace is always lowest priority.
      T![nl] => 0,

      _ => 1,
    })
    .unwrap();

  let parent = node.parent().unwrap();
  scalarc_syntax::match_ast! {
    match parent {
      ast::FieldExpr(f) => {
        let lhs = f.expr()?;
        // TODO: This is a bit dumb, but not all that dumb.
        let ty = db.type_at(pos.file, lhs.syntax().text_range().end())?;

        field_completions(db, pos.file, ty)
      },
      _ => Some(top_level_completions(db, pos))
    }
  }
}

fn field_completions(db: &RootDatabase, file_id: FileId, ty: Type) -> Option<Vec<Completion>> {
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

fn fields_of_def(db: &RootDatabase, def: &Definition) -> Option<Vec<Completion>> {
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

fn top_level_completions(db: &RootDatabase, pos: FileLocation) -> Vec<Completion> {
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
