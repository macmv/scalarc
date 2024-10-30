use scalarc_syntax::SyntaxNodePtr;

use super::{AstId, BindingKind, BlockId, ExprId, UnresolvedPath};
use crate::{
  hir, AnyDefinition, DefinitionKey, GlobalDefinitionKind, HirDatabase, HirDefinition,
  HirDefinitionId, HirDefinitionKind, InFile, InFileExt, Name, Path,
};

pub fn def_for_expr(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  expr: ExprId,
) -> Option<AnyDefinition> {
  let ast = db.hir_ast_for_block(block);

  // TODO: Maybe return some info about object vs class, based on what HIR
  // expression we're in.

  match ast.exprs[expr] {
    hir::Expr::Name(ref path) => {
      // TODO: Fix this? Most of the time there's just one segment, so we're going to
      // pull that out.
      let name = path.segments.last().unwrap();

      db.lookup_name_in_block(block, name.into())
    }

    hir::Expr::New(ref path, _) => {
      // TODO: Fix this? Most of the time there's just one segment, so we're going to
      // pull that out.
      let name = path.segments.last().unwrap();

      db.lookup_name_in_block(block, name.into())
    }

    hir::Expr::FieldAccess(lhs, ref field) => {
      let def = db.def_for_expr(block, lhs)?;

      // FIXME: This is copied from the typer. Need to dedupe.
      let block = match def {
        AnyDefinition::Hir(d) => {
          let parent_ast = db.hir_ast_for_block(d.block_id);
          match d.id {
            HirDefinitionId::Stmt(stmt) => {
              let hir::Stmt::Binding(ref binding) = parent_ast.stmts[stmt] else { return None };

              match binding.kind {
                BindingKind::Object(id) => BlockId::Object(id).in_file(d.block_id.file_id),
                _ => return None,
              }
            }
            _ => return None,
          }
        }
        AnyDefinition::Global(d) => match d.kind {
          GlobalDefinitionKind::Class(Some(_), _) => {
            BlockId::Class(AstId::new(d.ast_id)).in_file(d.file_id)
          }
          GlobalDefinitionKind::Trait(Some(_)) => {
            BlockId::Trait(AstId::new(d.ast_id)).in_file(d.file_id)
          }
          GlobalDefinitionKind::Object(Some(_)) => {
            BlockId::Object(AstId::new(d.ast_id)).in_file(d.file_id)
          }
          _ => return None,
        },
      };

      let hir_ast = db.hir_ast_for_block(block);

      // Find all the `def` and `val`s in the block.
      let decls: Vec<_> = hir_ast
        .items
        .iter()
        .filter_map(|&it| match hir_ast.stmts[it] {
          hir::Stmt::Binding(ref b) => {
            if b.name == *field {
              Some(it)
            } else {
              None
            }
          }
          _ => None,
        })
        .collect();

      match decls[..] {
        [] => None,
        [stmt_id] => Some(AnyDefinition::Hir(HirDefinition {
          name:     Name::new(field.clone()),
          id:       HirDefinitionId::Stmt(stmt_id),
          block_id: block,
          kind:     HirDefinitionKind::Val(None),
        })),
        _ => {
          // TODO: Resolve overloads.
          let stmt_id = decls.first().unwrap();

          Some(AnyDefinition::Hir(HirDefinition {
            name:     Name::new(field.clone()),
            id:       HirDefinitionId::Stmt(*stmt_id),
            block_id: block,
            kind:     HirDefinitionKind::Val(None),
          }))
        }
      }
    }

    _ => None,
  }
}

/// Looks up a `Name()` in a block. This will resolve bindings, parameters, and
/// object imports. It will not resolve instances.
pub fn lookup_name_in_block(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  name: String,
) -> Option<AnyDefinition> {
  let ast = db.hir_ast_for_block(block);

  for item in ast.items.iter() {
    if let hir::Stmt::Binding(ref binding) = ast.stmts[*item] {
      if binding.name == *name {
        return Some(HirDefinition::new_local(binding, block, HirDefinitionId::Stmt(*item)).into());
      }
    }
  }

  for (param, binding) in ast.params.iter() {
    if binding.name == *name {
      return Some(HirDefinition::new_param(binding, block, HirDefinitionId::Param(param)).into());
    }
  }

  for (import_id, import) in ast.imports.iter() {
    if import.wildcard {
      let mut p = import.path.clone();
      p.elems.push(Name::new(name.clone()));

      if let Some(target) = db.file_target(block.file_id) {
        if let Some(def) = db.definition_for_key(target, DefinitionKey::Object(p.clone())) {
          return Some(def.into());
        }
      }
    } else {
      let matches = match import.rename {
        Some(ref n) => n.as_str() == name,
        None => import.path.elems.last().unwrap().as_str() == name,
      };

      if matches {
        // Attempt to find the actual definition.
        if let Some(target) = db.file_target(block.file_id) {
          if let Some(def) =
            db.definition_for_key(target, DefinitionKey::Object(import.path.clone()))
          {
            return Some(def.into());
          }
        }

        // If not, just point at the import.
        return Some(
          HirDefinition {
            name:     import.path.elems.last().unwrap().clone(),
            id:       HirDefinitionId::Import(import_id),
            block_id: block,
            kind:     HirDefinitionKind::Import,
          }
          .into(),
        );
      }
    }
  }

  let parent_block = db.parent_block(block)?;
  db.lookup_name_in_block(parent_block.in_file(block.file_id), name)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolutionKind {
  Object,
  Instance,
}

impl ResolutionKind {
  pub fn make_key(self, path: Path) -> DefinitionKey {
    match self {
      ResolutionKind::Object => DefinitionKey::Object(path),
      ResolutionKind::Instance => DefinitionKey::Instance(path),
    }
  }
}

pub fn resolve_path_in_block(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  path: UnresolvedPath,
  kind: ResolutionKind,
) -> Option<Path> {
  let ast = db.hir_ast_for_block(block);

  let name = path.segments.first().unwrap();

  for import in ast.imports.values() {
    if import.wildcard {
      let mut p = import.path.clone();
      p.elems.extend(path.segments.clone().into_iter().map(|s| s.into()));

      if let Some(target) = db.file_target(block.file_id) {
        if db.definition_for_key(target, kind.make_key(p.clone())).is_some() {
          return Some(p);
        }
      }
    } else {
      let matches = match import.rename {
        Some(ref n) => n.as_str() == name,
        None => import.path.elems.last().unwrap().as_str() == name,
      };

      if matches {
        return Some(import.path.clone());
      }
    }
  }

  match db.parent_block(block) {
    Some(p) => db.resolve_path_in_block(p.in_file(block.file_id), path, kind),
    None => {
      // Now that we've reached the top level, we know the name doesn't resolve
      // anywhere. So, attempt to find a package-local item.

      let mut package = db.package_for_file(block.file_id).unwrap_or_default();
      package.elems.extend(path.segments.clone().into_iter().map(|s| s.into()));
      let target = db.file_target(block.file_id)?;
      if db.definition_for_key(target, kind.make_key(package.clone())).is_some() {
        return Some(package);
      }

      // The scala spec defines the following implicit imports:
      // - `scala.Predef._`
      // - `scala._`
      // - `java.lang._`
      //
      // So we'll try those in order.

      let mut scala = Path { elems: vec!["scala".into(), "Predef".into()] };
      scala.elems.extend(path.segments.clone().into_iter().map(|s| s.into()));

      if db.definition_for_key(target, kind.make_key(scala.clone())).is_some() {
        return Some(scala);
      }

      let mut scala = Path { elems: vec!["scala".into()] };
      scala.elems.extend(path.segments.clone().into_iter().map(|s| s.into()));

      if db.definition_for_key(target, kind.make_key(scala.clone())).is_some() {
        return Some(scala);
      }

      let mut java = Path { elems: vec!["java".into()] };
      java.elems.extend(path.segments.into_iter().map(|s| s.into()));

      if db.definition_for_key(target, kind.make_key(java.clone())).is_some() {
        return Some(java);
      }

      None
    }
  }
}

pub fn parent_block(db: &dyn HirDatabase, block: InFile<BlockId>) -> Option<BlockId> {
  let ast_id = block.id.erased();
  let ast = db.parse(block.file_id);
  let ast_id_map = db.ast_id_map(block.file_id);
  let mut ptr = ast_id_map.get_erased(ast_id).clone();

  loop {
    let outer_block = db.block_for_node(ptr.in_file(block.file_id));
    if outer_block != block {
      return Some(outer_block.id);
    }

    let node = ptr.to_node(&ast.syntax_node());
    let parent = node.parent()?;
    ptr = SyntaxNodePtr::new(&parent);
  }
}
