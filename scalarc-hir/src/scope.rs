use std::sync::Arc;

use la_arena::Arena;
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{AstNode, SyntaxKind},
  node::{NodeOrToken, SyntaxNode, SyntaxToken},
};

use crate::{Definition, DefinitionKind, FileRange, HirDatabase, LocalDefinition};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  pub declarations: Vec<(String, Definition)>,
}

impl Scope {
  fn is_empty(&self) -> bool { self.declarations.is_empty() }
}

pub fn scopes_of(db: &dyn HirDatabase, file_id: FileId) -> Arena<Scope> {
  // Breadth-first search of all scopes in the given file.
  let ast = db.parse(file_id);

  // FIXME: This return structure is useful for caching, but difficult to work
  // with. Given a point in the syntax tree, I don't know how to find the
  // current scope I'm in, much less all the parent scopes.
  let mut scopes = Arena::new();

  let tree = ast.tree();
  let mut this_pass = vec![tree.syntax().clone()];
  let mut next_pass = vec![];
  while !this_pass.is_empty() {
    for item in this_pass.drain(..) {
      let scope = single_scope(file_id, &item);
      if !scope.is_empty() {
        scopes.alloc(scope);
      }
      for node in item.children() {
        if has_children(&node) {
          next_pass.push(node);
        }
      }
    }
    std::mem::swap(&mut this_pass, &mut next_pass);
  }

  scopes
}

fn has_children(node: &SyntaxNode) -> bool {
  match node.kind() {
    SyntaxKind::VAL_DEF => true,
    SyntaxKind::FUN_DEF => true,
    SyntaxKind::BLOCK_EXPR => true,
    _ => false,
  }
}

fn single_scope(file_id: FileId, n: &SyntaxNode) -> Scope {
  let mut declarations = vec![];

  for n in n.children() {
    match n.kind() {
      SyntaxKind::VAL_DEF => {
        let n = scalarc_syntax::ast::ValDef::cast(n.clone()).unwrap();
        if let Some(id) = n.id_token() {
          declarations.push((
            id.text().into(),
            Definition {
              pos:  FileRange { file: file_id, range: id.text_range() },
              kind: DefinitionKind::Local(LocalDefinition::Val),
            },
          ));
        }
      }

      _ => {}
    }
  }

  Scope { declarations }
}

// Returns the scopes around the given token. The first scope is the innermost.
pub fn scopes_for(file_id: FileId, token: &SyntaxToken) -> Vec<Scope> {
  let mut scopes = vec![];

  let mut n: NodeOrToken = token.clone().into();

  loop {
    scopes.push(collect_scope(file_id, &n));

    if let Some(parent) = n.parent() {
      n = parent.into();
    } else {
      break;
    }
  }

  scopes
}

fn collect_scope(file_id: FileId, t: &NodeOrToken) -> Scope {
  let mut declarations = vec![];

  for n in iter_prev_siblings(t) {
    match n.kind() {
      SyntaxKind::VAL_DEF => {
        let n = scalarc_syntax::ast::ValDef::cast(n).unwrap();
        if let Some(id) = n.id_token() {
          declarations.push((
            id.text().into(),
            Definition {
              pos:  FileRange { file: file_id, range: id.text_range() },
              kind: DefinitionKind::Local(LocalDefinition::Val),
            },
          ));
        }
      }

      SyntaxKind::FUN_SIG => {
        let sig = scalarc_syntax::ast::FunSig::cast(n).unwrap();

        for params in sig.fun_paramss() {
          for param in params.fun_params() {
            if let Some(id) = param.id_token() {
              declarations.push((
                id.text().into(),
                Definition {
                  pos:  FileRange { file: file_id, range: id.text_range() },
                  kind: DefinitionKind::Local(LocalDefinition::Parameter),
                },
              ));
            }
          }
        }
      }

      _ => {}
    }
  }

  declarations.reverse();

  Scope { declarations }
}

fn iter_prev_siblings(t: &NodeOrToken) -> impl Iterator<Item = SyntaxNode> {
  struct Iter {
    prev: NodeOrToken,
  }

  impl Iterator for Iter {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<SyntaxNode> {
      loop {
        let next = self.prev.prev_sibling_or_token()?;
        self.prev = next.clone();
        if let Some(node) = next.as_node() {
          return Some(node.clone());
        }
      }
    }
  }

  Iter { prev: t.clone() }
}
