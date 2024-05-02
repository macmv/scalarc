use la_arena::{Arena, Idx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{AstNode, SyntaxKind},
  node::SyntaxNode,
  TextRange, TextSize, T,
};

use crate::{
  tree::Name, Definition, DefinitionKind, FileRange, GlobalDefinition, HirDatabase,
  LocalDefinition, Path,
};

pub type ScopeId = Idx<Scope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  /// The parent of this scope. `None` if this is the top-level scope of a file.
  pub parent: Option<ScopeId>,

  /// The range of text at which this scope is visible in.
  ///
  /// This might not be the same as the range of the node that defines the
  /// scope. For example, class parameters are only visible within the body of
  /// the class. So `visible` will be the class body for the scope that class
  /// paramters define.
  pub visible: TextRange,

  /// All the names declared by the scope.
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
  let Some(innermost) = scopes.iter().rev().find(|(_, scope)| scope.visible.contains(pos)) else {
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

  let mut this_pass = vec![(vec![tree.syntax().clone()], None, tree.syntax().text_range())];
  let mut next_pass = vec![];
  while !this_pass.is_empty() {
    for (items, parent, visible) in this_pass.drain(..) {
      info!("checking {items:?} visible at {visible:?}");

      let mut scope = Scope { parent, visible, declarations: vec![] };
      for item in items.iter() {
        scope.declarations.extend(single_scope(file_id, &item, visible).declarations);
      }
      let id = if !scope.is_empty() { Some(scopes.alloc(scope)) } else { parent };

      for node in items.iter().flat_map(|it| it.children()) {
        let visible = node.text_range();

        match node.kind() {
          SyntaxKind::VAL_DEF => {
            let n = scalarc_syntax::ast::ValDef::cast(node.clone()).unwrap();

            let Some(expr) = n.expr() else { continue };

            next_pass.push((vec![expr.syntax().clone()], id, visible));
          }
          SyntaxKind::BLOCK_EXPR => {
            next_pass.push((vec![node], id, visible));
          }
          SyntaxKind::CLASS_DEF => {
            info!("got class def");
            let n = scalarc_syntax::ast::ClassDef::cast(node.clone()).unwrap();

            // Walk one level deeper manually, so that parameters and defs in the body are
            // both visible in only the class body.
            let Some(params) = n.fun_params() else { continue };
            let Some(body) = n.body() else { continue };

            next_pass.push((
              vec![params.syntax().clone(), body.syntax().clone()],
              id,
              body.syntax().text_range(),
            ));
          }
          _ => continue,
        };
      }
    }
    std::mem::swap(&mut this_pass, &mut next_pass);
  }

  scopes
}

fn single_scope(file_id: FileId, n: &SyntaxNode, visible: TextRange) -> Scope {
  let mut declarations = vec![];

  info!("checking children of {n:?}");
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

      SyntaxKind::CLASS_DEF => {
        let n = scalarc_syntax::ast::ClassDef::cast(n.clone()).unwrap();
        if let Some(id) = n.id_token() {
          declarations.push((
            id.text().into(),
            Definition {
              pos:  FileRange { file: file_id, range: id.text_range() },
              name: id.text().into(),
              kind: DefinitionKind::Global(GlobalDefinition::Class),
            },
          ));
        }
      }

      SyntaxKind::FUN_PARAM => {
        info!("found fun param {n:#?}");
        let n = scalarc_syntax::ast::FunParam::cast(n.clone()).unwrap();
        if let Some(id) = n.id_token() {
          declarations.push((
            id.text().into(),
            Definition {
              pos:  FileRange { file: file_id, range: id.text_range() },
              name: id.text().into(),
              kind: DefinitionKind::Local(LocalDefinition::Parameter),
            },
          ));
        }
      }

      _ => {}
    }
  }

  Scope { parent: None, visible, declarations }
}
