use std::mem;

use la_arena::{Arena, Idx, RawIdx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{AstNode, Item, SyntaxKind},
  node::SyntaxNode,
  TextSize, T,
};

use crate::{
  ast::ErasedScopeId, tree::Name, Definition, DefinitionKind, FileRange, HirDatabase, Params, Path,
  Reference, Signature, Type,
};

pub type ScopeId = Idx<Scope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  /// The parent of this scope. `None` if this is the top-level scope of a file.
  pub parent: Option<ScopeId>,

  /// The erased item this scope is defined in.
  pub item_id: ErasedScopeId,

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
  let file_scopes = db.scopes_of(file_id);
  let item_id_map = db.item_id_map(file_id);

  let mut defs = vec![];

  // Find the last (ie, smallest) scope that contains the given span.
  let Some(innermost) = file_scopes.scopes.iter().rev().find(|(_, scope)| {
    let item = item_id_map.get_erased(scope.item_id);
    item.text_range().contains_inclusive(pos)
  }) else {
    return vec![];
  };

  // Now collect all the parents of that scope.
  let mut scope = innermost.1;
  defs.extend(scope.declarations.iter().rev().filter_map(|(_, def)| {
    let item = item_id_map.get_erased(def.item_id);

    if item.text_range().end() <= pos {
      Some(def.clone())
    } else {
      None
    }
  }));
  while let Some(parent) = scope.parent {
    scope = &file_scopes.scopes[parent];
    defs.extend(scope.declarations.iter().rev().filter_map(|(_, def)| {
      let item = item_id_map.get_erased(def.item_id);

      if item.text_range().end() <= pos {
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
  let item_id_map = db.item_id_map(file_id);

  let mut scopes = Arena::new();

  let tree = ast.tree();

  for (item_id, item) in item_id_map.iter() {
    let item = item.to_node(tree.syntax());
    let parent = None; // FIXME

    let scope_id = Idx::<Scope>::from_raw(RawIdx::from(scopes.len() as u32));
    let mut scope = Scope { parent, item_id, declarations: vec![] };

    if let Some(it) = Item::cast(item.clone()) {
      match it {
        Item::ClassDef(c) => {
          if let Some(p) = c.fun_params() {
            scope.declarations.extend(definitions_of(db, file_id, p.syntax(), scope_id));
          }
          if let Some(body) = c.body() {
            scope.declarations.extend(definitions_of(db, file_id, body.syntax(), scope_id));
          }
        }
        Item::FunDef(c) => {
          if let Some(p) = c.fun_sig() {
            for params in p.fun_paramss() {
              scope.declarations.extend(definitions_of(db, file_id, params.syntax(), scope_id));
            }
          }
        }
        _ => {}
      }
    } else {
      scope.declarations.extend(definitions_of(db, file_id, &item, scope_id));
    }

    if !scope.is_empty() {
      scopes.alloc(scope);
    }
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
      let item_id = db.item_id_map(file_id).erased_item_id(&n);

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
        parent_scope: scope,
        item_id,
        kind: DefinitionKind::Val(ty),
      })
    }

    SyntaxKind::CLASS_DEF => {
      let item_id = db.item_id_map(file_id).erased_item_id(&n);

      let c = scalarc_syntax::ast::ClassDef::cast(n.clone()).unwrap();
      let id = c.id_token()?;
      Some(Definition {
        name: id.text().into(),
        parent_scope: scope,
        item_id,
        kind: DefinitionKind::Class,
      })
    }

    SyntaxKind::FUN_DEF => {
      let item_id = db.item_id_map(file_id).erased_item_id(&n);

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
        parent_scope: scope,
        item_id,
        kind: DefinitionKind::Def(hir_sig),
      })
    }

    // TODO: This is `FUN_PARAM` for both functions and classes right now. It should be updated
    // to `CLASS_PARAM`, as those can define `val`s on the class.
    SyntaxKind::FUN_PARAM => {
      let item_id = db.item_id_map(file_id).erased_item_id(&n);

      let p = scalarc_syntax::ast::FunParam::cast(n.clone()).unwrap();

      let id = p.id_token()?;
      Some(Definition {
        name: id.text().into(),
        parent_scope: scope,
        item_id,
        kind: DefinitionKind::Parameter,
      })
    }

    _ => None,
  }
}

pub fn references_to(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Vec<Reference> {
  let ast = db.parse(file_id);
  let item_id_map = db.item_id_map(file_id);
  let tree = ast.tree();

  let file_scopes = db.scopes_of(file_id);

  let Some(def) = db.def_at_index(file_id, pos) else { return vec![] };

  let scope = &file_scopes.scopes[def.parent_scope];

  let mut references = vec![];

  let item = item_id_map.get_erased(scope.item_id);
  let mut this_pass: Vec<_> = vec![item.to_node(tree.syntax())];
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
