use la_arena::{Arena, Idx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{AstNode, SyntaxKind},
  node::{NodeOrToken, SyntaxNode, SyntaxToken},
  TextRange, TextSize, T,
};

use crate::{
  tree::Name, Definition, DefinitionKind, FileRange, HirDatabase, LocalDefinition, Path,
};

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

pub fn def_at_index(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Option<Definition> {
  let defs = db.defs_at_index(file_id, pos);

  let ast = db.parse(file_id);

  let node = ast
    .syntax_node()
    .token_at_offset(pos)
    .max_by_key(|token| match token.kind() {
      T![ident] => 10,
      _ => 1,
    })
    .unwrap();

  match node.kind() {
    T![ident] => {
      let name = Name::new(node.text().to_string());

      // Scopes are ordered innermost to outermost, so the first definition we find is
      // the one we want.
      if let Some(def) = defs.iter().find(|def| def.name.as_str() == name.as_str()) {
        return Some(def.clone());
      }

      let hir = db.hir_ast(file_id);
      let path = match hir.imports.get(&name) {
        Some(path) => path.clone(),

        // TODO: Use the local package name here. That should be in the HIR ast.
        None => Path { elems: vec![name] },
      };

      let source_root = db.file_source_root(file_id);
      let target = db.source_root_target(source_root);
      let definitions = db.definitions_for_target(target);

      definitions.items.get(&path).cloned()
    }

    _ => None,
  }
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
              name: id.text().into(),
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
