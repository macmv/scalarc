use std::collections::HashSet;

use scalarc_hir::{scope::Declaration, HirDatabase};
use scalarc_parser::T;
use scalarc_source::SourceDatabase;

use crate::{database::RootDatabase, FileLocation};

pub struct Completion {
  pub label: String,
  pub kind:  Declaration,
}

pub fn completions(db: &RootDatabase, pos: FileLocation) -> Vec<Completion> {
  let mut completions = vec![];

  info!("finding completions...");

  let source_root = db.file_source_root(pos.file);
  let target = db.source_root_target(source_root);
  let definitions = db.definitions_for_target(target);

  for (mut path, _) in definitions.items {
    completions.push(Completion {
      label: path.elems.pop().unwrap().into_string(),
      kind:  Declaration::Class,
    });
  }

  let ast = db.parse(pos.file);

  let node = ast
    .syntax_node()
    .token_at_offset(pos.index)
    .max_by_key(|token| match token.kind() {
      T![ident] => 10,
      _ => 1,
    })
    .unwrap();

  let scopes = scalarc_hir::scope::scopes_for(&node);

  let mut names = HashSet::new();
  for scope in scopes {
    for (name, kind) in scope.declarations {
      if names.insert(name.clone()) {
        completions.push(Completion { label: name, kind });
      }
    }
  }

  info!("got {} completions!", completions.len());

  completions
}
