use std::collections::HashMap;

use la_arena::{Arena, Idx, RawIdx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode, Item, ItemBody, SyntaxKind},
  match_ast,
  node::{SyntaxNode, SyntaxToken},
  AstPtr, SyntaxNodePtr, TextSize, T,
};

use crate::{
  hir::{self, AstId, BlockId, ErasedAstId},
  AnyDefinition, ClassKind, DefinitionKey, FileRange, GlobalDefinition, GlobalDefinitionKind,
  HirDatabase, HirDefinition, HirDefinitionId, InFile, InFileExt, Name, Path, Reference,
};

pub type ScopeId = Idx<Scope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  /// The parent of this scope. `None` if this is the top-level scope of a file.
  pub parent: Option<ScopeId>,

  /// The erased item this scope is defined in.
  pub ast_id: ErasedAstId,

  /// All the names declared by the scope.
  pub declarations: Vec<(String, GlobalDefinition)>,
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

pub fn def_at_index(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Option<AnyDefinition> {
  let ast = db.parse(file_id);

  let token = ast
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

  let mut n = token.parent()?;
  loop {
    match_ast! {
      match n {
        ast::Expr(e) => return expr_definition(db, file_id, &e),
        ast::Item(it) => return Some(AnyDefinition::Hir(item_definition(db, file_id, &it, pos)?)),
        ast::FunParam(p) => return Some(AnyDefinition::Hir(param_definition(db, file_id, &p, pos)?)),
        ast::Import(_) => return Some(AnyDefinition::Global(import_definition(db, file_id, token)?)),
        _ => n = n.parent()?,
      }
    }
  }
}

fn expr_definition(
  db: &dyn HirDatabase,
  file_id: FileId,
  expr: &ast::Expr,
) -> Option<AnyDefinition> {
  let ptr = AstPtr::new(expr);
  let syntax_ptr = SyntaxNodePtr::new(&expr.syntax());
  let block = db.block_for_node(syntax_ptr.in_file(file_id));
  let Some(expr_id) = db.hir_source_map_for_block(block).expr(ptr) else {
    // TODO: This shouldn't happen? Need to debug.
    return None;
  };

  db.def_for_expr(block, expr_id)
}

fn item_definition(
  db: &dyn HirDatabase,
  file_id: FileId,
  stmt: &ast::Item,
  pos: TextSize,
) -> Option<HirDefinition> {
  match stmt {
    ast::Item::FunDef(d) => {
      if let Some(sig) = d.fun_sig() {
        if let Some(id) = sig.id_token() {
          if id.text_range().contains_inclusive(pos) {
            return item_definition_real(db, file_id, stmt);
          }
        }
      }
    }
    ast::Item::ValDef(def) => {
      // FIXME: Re-implement pattern vals.
      /*
      match d.pattern()? {
        ast::Pattern::PathPattern(p) => {
          if let Some(id) = p.path()?.ids().next() {
            if id.text_range().contains_inclusive(pos) {
              return item_definition_real(db, file_id, stmt);
            }
          }
        }
        ast::Pattern::TypePattern(t) => {
          if let Some(id) = t.id_token() {
            if id.text_range().contains_inclusive(pos) {
              return item_definition_real(db, file_id, stmt);
            }
          }
        }
        // TODO: Handle patterns like `val (x, y) = ...`
        _ => {}
      }
      */
      if let Some(id) = def.id_token() {
        if id.text_range().contains_inclusive(pos) {
          return item_definition_real(db, file_id, stmt);
        }
      }
    }
    _ => {}
  }

  None
}

fn item_definition_real(
  db: &dyn HirDatabase,
  file_id: FileId,
  stmt: &ast::Item,
) -> Option<HirDefinition> {
  let syntax_ptr = SyntaxNodePtr::new(&stmt.syntax().parent()?);
  let block = db.block_for_node(syntax_ptr.in_file(file_id));

  let ptr = AstPtr::new(stmt);
  let item_id = db.hir_source_map_for_block(block).stmt(ptr)?;

  match db.hir_ast_for_block(block).stmts[item_id] {
    hir::Stmt::Binding(ref binding) => {
      Some(HirDefinition::new_local(binding, block, HirDefinitionId::Stmt(item_id)))
    }
    _ => None,
  }
}

fn param_definition(
  db: &dyn HirDatabase,
  file_id: FileId,
  param: &ast::FunParam,
  pos: TextSize,
) -> Option<HirDefinition> {
  if let Some(id) = param.id_token() {
    if id.text_range().contains_inclusive(pos) {
      return param_definition_real(db, file_id, param);
    }
  }

  None
}

fn param_definition_real(
  db: &dyn HirDatabase,
  file_id: FileId,
  param: &ast::FunParam,
) -> Option<HirDefinition> {
  // Params are nested in `FUN_PARAMS` and then `FUN_SIG`.
  let def = param.syntax().parent()?.parent()?;

  let syntax_ptr = SyntaxNodePtr::new(&def);
  let block = db.block_for_node(syntax_ptr.in_file(file_id));

  let ptr = SyntaxNodePtr::new(param.syntax());
  let binding_id = db.hir_source_map_for_block(block).param(ptr)?;

  let binding = &db.hir_ast_for_block(block).params[binding_id];

  Some(HirDefinition::new_local(binding, block, HirDefinitionId::Param(binding_id)))
}

fn import_definition(
  db: &dyn HirDatabase,
  file_id: FileId,
  token: SyntaxToken,
) -> Option<GlobalDefinition> {
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
) -> impl Iterator<Item = (String, GlobalDefinition)> + 'a {
  n.children().filter_map(move |n| {
    let def = def_of_node(db, file_id, scope, n);
    def.map(|def| (def.name().as_str().into(), def))
  })
}

// FIXME: Defs should really be per-scope, not per-file.
fn def_of_node(
  db: &dyn HirDatabase,
  file_id: FileId,
  _scope: ScopeId,
  n: SyntaxNode,
) -> Option<GlobalDefinition> {
  let mut path = db.package_for_file(file_id).unwrap_or_default();

  match n.kind() {
    SyntaxKind::CLASS_DEF => {
      let ast_id_map = db.ast_id_map(file_id);
      let ast_id = ast_id_map.erased_ast_id(&n);

      let c = scalarc_syntax::ast::ClassDef::cast(n.clone()).unwrap();
      let id = c.id_token()?;
      path.elems.push(id.text().into());

      Some(GlobalDefinition {
        path,
        file_id,
        ast_id,
        kind: GlobalDefinitionKind::Class(
          c.body().map(|node| ast_id_map.ast_id(&node)),
          if c.case_token().is_some() { ClassKind::Case } else { ClassKind::Normal },
        ),
      })
    }

    SyntaxKind::TRAIT_DEF => {
      let ast_id_map = db.ast_id_map(file_id);
      let ast_id = ast_id_map.erased_ast_id(&n);

      let c = scalarc_syntax::ast::TraitDef::cast(n.clone()).unwrap();
      let id = c.id_token()?;
      path.elems.push(id.text().into());

      Some(GlobalDefinition {
        path,
        file_id,
        ast_id,
        kind: GlobalDefinitionKind::Trait(c.body().map(|node| ast_id_map.ast_id(&node))),
      })
    }

    SyntaxKind::OBJECT_DEF => {
      let ast_id_map = db.ast_id_map(file_id);
      let ast_id = ast_id_map.erased_ast_id(&n);

      let c = scalarc_syntax::ast::ObjectDef::cast(n.clone()).unwrap();
      let id = c.id_token()?;
      path.elems.push(id.text().into());

      Some(GlobalDefinition {
        path,
        file_id,
        ast_id,
        kind: GlobalDefinitionKind::Object(c.body().map(|node| ast_id_map.ast_id(&node))),
      })
    }

    _ => None,
  }
}

pub fn references_to(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Vec<Reference> {
  let Some(def) = db.def_at_index(file_id, pos) else { return vec![] };

  match def {
    AnyDefinition::Hir(def) => {
      let mut refs = vec![];

      walk_references(db, &def, def.block_id, &mut refs);

      refs
    }

    AnyDefinition::Global(_) => {
      return vec![];
    }
  }
}

fn walk_references(
  db: &dyn HirDatabase,
  def: &HirDefinition,
  block: InFile<BlockId>,
  refs: &mut Vec<Reference>,
) {
  fn walk_block(
    db: &dyn HirDatabase,
    def: &HirDefinition,
    name: &str,
    block: InFile<BlockId>,
    refs: &mut Vec<Reference>,
  ) {
    let inner_block = db.hir_ast_for_block(block);

    // This might seem wrong, but scala doesn't allow redeclaring values in the same
    // block. So this actually handles all the cases we care about.
    let redeclared = inner_block.bindings().any(|b| b.name == name);

    if !redeclared {
      walk_references(db, def, block, refs);
    }
  }

  for (expr_id, expr) in db.hir_ast_for_block(block).exprs.iter() {
    match &expr {
      hir::Expr::Name(ref path) => {
        if path.segments.last().unwrap() == def.name.as_str() {
          refs.push(Reference {
            pos: FileRange {
              file:  block.file_id,
              range: db
                .hir_source_map_for_block(block)
                .expr_syntax(expr_id)
                .unwrap()
                .to_node(&db.parse(block.file_id))
                .text_range(),
            },
          });
        }
      }

      hir::Expr::Block(b) => {
        walk_block(db, def, def.name.as_str(), (*b).in_file(block.file_id), refs);
      }

      hir::Expr::InterpolatedString(s) => {
        for intp in s.iter() {
          match intp {
            hir::Interpolation::Block(b) => {
              walk_block(db, def, def.name.as_str(), (*b).in_file(block.file_id), refs);
            }
            _ => {}
          }
        }
      }

      _ => {}
    }
  }
}
