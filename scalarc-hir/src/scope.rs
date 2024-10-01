use std::{collections::HashMap, mem};

use la_arena::{Arena, Idx, RawIdx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode, Item, ItemBody, SyntaxKind},
  match_ast,
  node::{SyntaxNode, SyntaxToken},
  TextSize, T,
};

use crate::{
  hir::{AstId, ErasedAstId},
  Definition, DefinitionKey, DefinitionKind, FileRange, HirDatabase, Name, Path, Reference,
  Signature, Type,
};

pub type ScopeId = Idx<Scope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  /// The parent of this scope. `None` if this is the top-level scope of a file.
  pub parent: Option<ScopeId>,

  /// The erased item this scope is defined in.
  pub ast_id: ErasedAstId,

  /// All the names declared by the scope.
  pub declarations: Vec<(String, Definition)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileScopes {
  pub scopes:       Arena<Scope>,
  pub ast_to_scope: HashMap<ErasedAstId, ScopeId>,
}

impl Scope {
  fn is_empty(&self) -> bool { self.declarations.is_empty() }
}

impl FileScopes {
  pub fn get(&self, ast_id: AstId<ItemBody>) -> Option<&Scope> {
    self.ast_to_scope.get(&ast_id.erased()).map(|id| &self.scopes[*id])
  }
}

/// Returns the definitions at the given scope. The innermost declarations (ie,
/// closest to the cursor) show up first in the list.
pub fn defs_at_index(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Vec<Definition> {
  let file_scopes = db.scopes_of(file_id);
  let ast_id_map = db.ast_id_map(file_id);

  let mut defs = vec![];

  // Find the last (ie, smallest) scope that contains the given span.
  let Some(innermost) = file_scopes.scopes.iter().rev().find(|(_, scope)| {
    let item = ast_id_map.get_erased(scope.ast_id);
    item.text_range().contains_inclusive(pos)
  }) else {
    return vec![];
  };

  // Now collect all the parents of that scope.
  let mut scope = innermost.1;
  defs.extend(scope.declarations.iter().rev().filter_map(|(_, def)| {
    let item = ast_id_map.get_erased(def.ast_id);

    if item.text_range().end() <= pos {
      Some(def.clone())
    } else {
      None
    }
  }));
  while let Some(parent) = scope.parent {
    scope = &file_scopes.scopes[parent];
    defs.extend(scope.declarations.iter().rev().filter_map(|(_, def)| {
      let item = ast_id_map.get_erased(def.ast_id);

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

  let mut n = node.parent()?;
  loop {
    match_ast! {
      match n {
        ast::Expr(_) => return expr_definition(db, file_id, pos, node),
        ast::Import(_) => return import_definition(db, file_id, node),
        _ => {}
      }
    }

    n = n.parent()?;
  }
}

fn expr_definition(
  db: &dyn HirDatabase,
  file_id: FileId,
  pos: TextSize,
  token: SyntaxToken,
) -> Option<Definition> {
  let defs = db.defs_at_index(file_id, pos);

  match token.kind() {
    T![ident] => {
      let name = Name::new(token.text().to_string());

      // Scopes are ordered innermost to outermost, so the first definition we find is
      // the one we want.
      if let Some(def) = defs.iter().find(|def| def.name.as_str() == name.as_str()) {
        return Some(def.clone());
      }

      // FIXME: Need global name lookup.
      //
      /*
      let hir = db.hir_ast(file_id);
      match hir.imports.get(&name) {
        Some(path) => path.clone(),
      };
      */
      let _path = Path { elems: vec![name] };

      let source_root = db.file_source_root(file_id)?;
      let target = db.source_root_target(source_root);
      let _definitions = db.definitions_for_target(target);

      // FIXME: Re-implement.
      // definitions.items.get(&path).cloned()
      None
    }

    _ => None,
  }
}

fn import_definition(
  db: &dyn HirDatabase,
  file_id: FileId,
  token: SyntaxToken,
) -> Option<Definition> {
  let parent = token.parent()?;
  info!("looking up import definition for {parent:#?}");
  match_ast! {
    match parent {
      ast::Path(p) => {
        let mut path = Path::new();

        for id in p.ids() {
          path.elems.push(Name::new(id.text().to_string()));
        }

        info!("looking up definition for import {:?}", path);

        let target = db.file_target(file_id)?;
        let inst_def = db.definition_for_key(target, DefinitionKey::Instance(path.clone()));
        let obj_def = db.definition_for_key(target, DefinitionKey::Object(path));

        inst_def.or(obj_def)
      },
      ast::ImportSelector(i) => {
        let selectors = ast::ImportSelectors::cast(parent.parent()?)?;

        let mut path = Path::new();
        if let Some(p) = selectors.path() {
          for id in p.ids() {
            path.elems.push(Name::new(id.text().to_string()));
          }
        }

        match i {
          ast::ImportSelector::ImportSelectorId(s) => {
            if let Some(id) = s.id_token() {
              path.elems.push(Name::new(id.text().to_string()));
            }
          }
          _ => {}
        }

        let target = db.file_target(file_id)?;
        let inst_def = db.definition_for_key(target, DefinitionKey::Instance(path.clone()));
        let obj_def = db.definition_for_key(target, DefinitionKey::Object(path));

        inst_def.or(obj_def)
      },
      _ => None,
    }
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
  let ast_id_map = db.ast_id_map(file_id);

  let mut scopes: Arena<Scope> = Arena::new();
  let mut ast_to_scope = HashMap::new();

  let tree = ast.tree();

  for (ast_id, item) in ast_id_map.iter() {
    let item = item.to_node(tree.syntax());
    let mut parent = None;
    let mut it = item.clone();
    while let Some(p) = it.parent() {
      if ast_id_map.contains_node(&p) {
        let ast_id = ast_id_map.erased_ast_id(&p);
        // FIXME: Maybe use a map instead here?
        if let Some((scope_id, _)) = scopes.iter().find(|(_, s)| s.ast_id == ast_id) {
          parent = Some(scope_id);
          break;
        }
      }
      it = p;
    }

    let scope_id = Idx::<Scope>::from_raw(RawIdx::from(scopes.len() as u32));
    let mut scope = Scope { parent, ast_id, declarations: vec![] };

    if let Some(it) = Item::cast(item.clone()) {
      match it {
        Item::ClassDef(c) => {
          if let Some(p) = c.fun_params() {
            scope.declarations.extend(definitions_of(db, file_id, p.syntax(), scope_id));
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
      ast_to_scope.insert(ast_id, scope_id);
    }
  }

  FileScopes { scopes, ast_to_scope }
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
      let ast_id = db.ast_id_map(file_id).erased_ast_id(&n);

      let v = scalarc_syntax::ast::ValDef::cast(n.clone()).unwrap();
      let id = v.id_token()?;

      let ty =
        v.ty().map(|ty| Type::Instance(Path { elems: vec![Name(ty.syntax().text().into())] }));

      Some(Definition {
        name: id.text().into(),
        file_id,
        parent_scope: scope,
        ast_id,
        kind: DefinitionKind::Val(ty),
      })
    }

    SyntaxKind::CLASS_DEF => {
      let ast_id_map = db.ast_id_map(file_id);
      let ast_id = ast_id_map.erased_ast_id(&n);

      let c = scalarc_syntax::ast::ClassDef::cast(n.clone()).unwrap();
      let id = c.id_token()?;

      Some(Definition {
        name: id.text().into(),
        file_id,
        parent_scope: scope,
        ast_id,
        kind: DefinitionKind::Class(c.body().map(|node| ast_id_map.ast_id(&node))),
      })
    }

    SyntaxKind::TRAIT_DEF => {
      let ast_id_map = db.ast_id_map(file_id);
      let ast_id = ast_id_map.erased_ast_id(&n);

      let c = scalarc_syntax::ast::TraitDef::cast(n.clone()).unwrap();
      let id = c.id_token()?;

      Some(Definition {
        name: id.text().into(),
        file_id,
        parent_scope: scope,
        ast_id,
        kind: DefinitionKind::Trait(c.body().map(|node| ast_id_map.ast_id(&node))),
      })
    }

    SyntaxKind::OBJECT_DEF => {
      let ast_id_map = db.ast_id_map(file_id);
      let ast_id = ast_id_map.erased_ast_id(&n);

      let c = scalarc_syntax::ast::ObjectDef::cast(n.clone()).unwrap();
      let id = c.id_token()?;

      Some(Definition {
        name: id.text().into(),
        file_id,
        parent_scope: scope,
        ast_id,
        kind: DefinitionKind::Object(c.body().map(|node| ast_id_map.ast_id(&node))),
      })
    }

    SyntaxKind::FUN_DEF => {
      let ast_id = db.ast_id_map(file_id).erased_ast_id(&n);

      let f = scalarc_syntax::ast::FunDef::cast(n.clone()).unwrap();

      let sig = f.fun_sig()?;
      let hir_sig = Signature::from_ast(&sig);

      let id = sig.id_token()?;

      Some(Definition {
        name: id.text().into(),
        file_id,
        parent_scope: scope,
        ast_id,
        kind: DefinitionKind::Def(hir_sig),
      })
    }

    // TODO: This is `FUN_PARAM` for both functions and classes right now. It should be updated
    // to `CLASS_PARAM`, as those can define `val`s on the class.
    SyntaxKind::FUN_PARAM => {
      let ast_id = db.ast_id_map(file_id).erased_ast_id(&n);

      let p = scalarc_syntax::ast::FunParam::cast(n.clone()).unwrap();

      let id = p.id_token()?;
      Some(Definition {
        name: id.text().into(),
        file_id,
        parent_scope: scope,
        ast_id,
        kind: DefinitionKind::Parameter,
      })
    }

    _ => None,
  }
}

pub fn references_to(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Vec<Reference> {
  let ast = db.parse(file_id);
  let ast_id_map = db.ast_id_map(file_id);
  let tree = ast.tree();

  let file_scopes = db.scopes_of(file_id);

  let Some(def) = db.def_at_index(file_id, pos) else { return vec![] };

  let scope = &file_scopes.scopes[def.parent_scope];

  let mut references = vec![];

  let item = ast_id_map.get_erased(scope.ast_id);
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
