use la_arena::{Arena, Idx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{AstNode, SyntaxKind},
  node::{NodeOrToken, SyntaxNode, SyntaxToken},
  TextRange, TextSize,
};

use crate::{Definition, DefinitionKind, FileRange, HirDatabase, LocalDefinition};

pub type ScopeId = Idx<Scope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  pub parent:       Option<ScopeId>,
  pub range:        TextRange,
  pub declarations: Vec<(String, Definition)>,
}

impl Scope {
  fn is_empty(&self) -> bool { self.declarations.is_empty() }
}

/// Returns the definitions at the given scope. The innermost declarations (ie,
/// closest to the cursor) show up first in the list.
pub fn defs_at_index(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Vec<Definition> {
  let scopes = db.scopes_of(file_id);

  let mut defs = vec![];

  // Find the last (ie, smallest) scope that contains the given span.
  dbg!(&scopes);
  let Some(innermost) = scopes.iter().rev().find(|(_, scope)| scope.range.contains(pos)) else {
    return vec![];
  };

  // Now collect all the parents of that scope.
  let mut scope = innermost.1;
  defs.extend(scope.declarations.iter().rev().filter_map(|(_, def)| {
    if def.pos.range.end() <= pos {
      Some(def.clone())
    } else {
      None
    }
  }));
  while let Some(parent) = scope.parent {
    scope = &scopes[parent];
    defs.extend(scope.declarations.iter().rev().filter_map(|(_, def)| {
      if def.pos.range.end() <= pos {
        Some(def.clone())
      } else {
        None
      }
    }));
  }

  defs
}

/// Returns all the scopes of the given file. This is in breadth-first order.
/// The indexes are stable across reparses, and are aliased to `ScopeId`.
///
/// Each scope contains a list of definitions (which could be values, classes,
/// functions, etc) in file-order.
pub fn scopes_of(db: &dyn HirDatabase, file_id: FileId) -> Arena<Scope> {
  // Breadth-first search of all scopes in the given file.
  let ast = db.parse(file_id);

  let mut scopes = Arena::new();

  let tree = ast.tree();
  let mut this_pass = vec![(tree.syntax().clone(), None)];
  let mut next_pass = vec![];
  while !this_pass.is_empty() {
    for (item, parent) in this_pass.drain(..) {
      let mut scope = single_scope(file_id, &item);
      scope.parent = parent;
      let id = if !scope.is_empty() { Some(scopes.alloc(scope)) } else { parent };
      for node in item.children() {
        if has_children(&node) {
          next_pass.push((node, id));
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

  Scope { parent: None, range: n.text_range(), declarations }
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

  Scope { parent: None, range: t.text_range(), declarations }
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
