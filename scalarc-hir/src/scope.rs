use std::mem;

use la_arena::{Arena, Idx, RawIdx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{AstNode, SyntaxKind},
  node::SyntaxNode,
  SyntaxNodePtr, TextRange, TextSize, T,
};

use crate::{
  tree::Name, Definition, DefinitionKind, FileRange, HirDatabase, LocalDefinition, Params, Path,
  Reference, Signature, Type,
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

  /// The roots of the scope. All definitions in the scope are children of these
  /// nodes.
  pub body: Vec<SyntaxNodePtr>,

  /// All the names declared by the scope.
  pub declarations: Vec<(String, Definition)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileScopes {
  pub scopes: Arena<Scope>,
}

impl Scope {
  fn is_empty(&self) -> bool { self.declarations.is_empty() }
}

/// Returns the definitions at the given scope. The innermost declarations (ie,
/// closest to the cursor) show up first in the list.
pub fn defs_at_index(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Vec<Definition> {
  // FIXME: This doesn't work with `ItemIdMap`. Need to rethink? Maybe?
  return vec![];

  let file_scopes = db.scopes_of(file_id);

  let mut defs = vec![];

  // Find the last (ie, smallest) scope that contains the given span.
  let Some(innermost) =
    file_scopes.scopes.iter().rev().find(|(_, scope)| scope.visible.contains_inclusive(pos))
  else {
    return vec![];
  };

  // Now collect all the parents of that scope.
  let mut scope = innermost.1;
  defs.extend(scope.declarations.iter().rev().filter_map(|(_, def)| {
    // FIXME
    /*
    if def.pos.range.end() <= pos {
      Some(def.clone())
    } else {
      None
    }
    */
    Some(def.clone())
  }));
  while let Some(parent) = scope.parent {
    scope = &file_scopes.scopes[parent];
    defs.extend(scope.declarations.iter().rev().filter_map(|(_, def)| {
      // FIXME
      /*
      if def.pos.range.end() <= pos {
        Some(def.clone())
      } else {
        None
      }
      */
      Some(def.clone())
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
      SyntaxKind::INT_LIT_KW => 9,

      // Whitespace is always lowest priority.
      T![nl] => 0,

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

      let source_root = db.file_source_root(file_id)?;
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
pub fn scopes_of(db: &dyn HirDatabase, file_id: FileId) -> FileScopes {
  // Breadth-first search of all scopes in the given file.
  let ast = db.parse(file_id);

  let mut scopes = Arena::new();

  let tree = ast.tree();

  let mut this_pass = vec![(vec![tree.syntax().clone()], None, tree.syntax().text_range())];
  let mut next_pass = vec![];
  while !this_pass.is_empty() {
    for (items, parent, parent_visible) in this_pass.drain(..) {
      let mut scope = Scope {
        parent,
        visible: parent_visible,
        body: items.iter().map(|n| SyntaxNodePtr::new(&n)).collect(),
        declarations: vec![],
      };
      let scope_id = Idx::from_raw(RawIdx::from_u32(scopes.len() as u32));
      for item in items.iter() {
        scope.declarations.extend(definitions_of(db, file_id, &item, scope_id));
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
          SyntaxKind::CLASS_DEF => {
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
          SyntaxKind::FUN_DEF => {
            let n = scalarc_syntax::ast::FunDef::cast(node.clone()).unwrap();

            let Some(sig) = n.fun_sig() else { continue };
            let Some(body) = n.expr() else { continue };

            next_pass.push((
              sig.syntax().children().chain([body.syntax().clone()]).collect(),
              id,
              body.syntax().text_range(),
            ));
          }
          SyntaxKind::EXPR_ITEM
          | SyntaxKind::BLOCK
          | SyntaxKind::BLOCK_EXPR
          | SyntaxKind::IF_EXPR
          | SyntaxKind::MATCH_EXPR
          | SyntaxKind::CASE_ITEM
          | SyntaxKind::CALL_EXPR
          | SyntaxKind::PAREN_ARGUMENTS
          | SyntaxKind::BLOCK_ARGUMENTS
          | SyntaxKind::SPREAD_ARGUMENTS => {
            next_pass.push((vec![node], id, visible));
          }
          _ => continue,
        };
      }
    }
    std::mem::swap(&mut this_pass, &mut next_pass);
  }

  FileScopes { scopes }
}

fn definitions_of<'a>(
  db: &'a dyn HirDatabase,
  file_id: FileId,
  n: &SyntaxNode,
  scope: ScopeId,
) -> impl Iterator<Item = (String, Definition)> + 'a {
  n.children().filter_map(move |n| {
    let def = def_of_node(db, file_id, scope, n);
    def.map(|def| (def.name.as_str().into(), def))
  })
}

fn def_of_node(
  db: &dyn HirDatabase,
  file_id: FileId,
  scope: ScopeId,
  n: SyntaxNode,
) -> Option<Definition> {
  match n.kind() {
    SyntaxKind::VAL_DEF => {
      let v = scalarc_syntax::ast::ValDef::cast(n.clone()).unwrap();
      let id = v.id_token()?;

      let ty = match v.ty() {
        Some(ty) => Some(Type { path: Path { elems: vec![Name(ty.syntax().text().into())] } }),
        None => v.expr().and_then(|e| {
          db.type_at(file_id, e.syntax().text_range().end()).map(|ty| Type { path: ty.path })
        }),
      };

      Some(Definition {
        name: id.text().into(),
        scope,
        kind: DefinitionKind::Local(LocalDefinition::Val(ty)),
      })
    }

    SyntaxKind::CLASS_DEF => {
      let c = scalarc_syntax::ast::ClassDef::cast(n.clone()).unwrap();
      let id = c.id_token()?;
      Some(Definition {
        name: id.text().into(),
        scope,
        kind: DefinitionKind::Local(LocalDefinition::Class),
      })
    }

    SyntaxKind::FUN_DEF => {
      let f = scalarc_syntax::ast::FunDef::cast(n.clone()).unwrap();

      let sig = f.fun_sig()?;

      let hir_sig = Signature {
        params: sig
          .fun_paramss()
          .map(|p| Params {
            implicit: false,
            params:   p
              .fun_params()
              .filter_map(|p| {
                if let (Some(id), Some(ty)) = (p.id_token(), p.ty()) {
                  Some((
                    id.text().into(),
                    Type { path: Path { elems: vec![Name(ty.syntax().text().into())] } },
                  ))
                } else {
                  None
                }
              })
              .collect(),
          })
          .collect(),
        ret:    None,
      };

      let id = sig.id_token()?;

      Some(Definition {
        name: id.text().into(),
        scope,
        kind: DefinitionKind::Local(LocalDefinition::Def(hir_sig)),
      })
    }

    // TODO: This is `FUN_PARAM` for both functions and classes right now. It should be updated
    // to `CLASS_PARAM`, as those can define `val`s on the class.
    SyntaxKind::FUN_PARAM => {
      let p = scalarc_syntax::ast::FunParam::cast(n.clone()).unwrap();
      let id = p.id_token()?;
      Some(Definition {
        name: id.text().into(),
        scope,
        kind: DefinitionKind::Local(LocalDefinition::Parameter),
      })
    }

    _ => None,
  }
}

pub fn references_to(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Vec<Reference> {
  let ast = db.parse(file_id);
  let tree = ast.tree();

  let file_scopes = db.scopes_of(file_id);

  let Some(def) = db.def_at_index(file_id, pos) else { return vec![] };

  let scope = &file_scopes.scopes[def.scope];

  let mut references = vec![];

  let mut this_pass: Vec<_> = scope.body.iter().map(|ptr| ptr.to_node(tree.syntax())).collect();
  let mut next_pass = vec![];

  while !this_pass.is_empty() {
    for node in this_pass.drain(..) {
      for child in node.children() {
        match child.kind() {
          SyntaxKind::IDENT_EXPR => {
            if child.text() == def.name.as_str() {
              references
                .push(Reference { pos: FileRange { file: file_id, range: child.text_range() } });
            }
          }

          SyntaxKind::BLOCK_EXPR
          | SyntaxKind::EXPR_ITEM
          | SyntaxKind::INFIX_EXPR
          | SyntaxKind::CALL_EXPR
          | SyntaxKind::PAREN_ARGUMENTS
          | SyntaxKind::IF_EXPR => {
            next_pass.push(child);
          }

          _ => {}
        }
      }
    }
    mem::swap(&mut this_pass, &mut next_pass);
  }

  references
}
