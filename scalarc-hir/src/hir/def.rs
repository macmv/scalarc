use scalarc_syntax::SyntaxNodePtr;

use super::{BlockId, ExprId, UnresolvedPath};
use crate::{
  hir, DefinitionKey, HirDatabase, HirDefinition, HirDefinitionId, HirDefinitionKind, InFile,
  InFileExt, Path,
};

pub fn def_for_expr(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  expr: ExprId,
) -> Option<HirDefinition> {
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

    _ => None,
  }
}

pub fn lookup_name_in_block(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  name: String,
) -> Option<HirDefinition> {
  let ast = db.hir_ast_for_block(block);

  for item in ast.items.iter() {
    if let hir::Stmt::Binding(ref binding) = ast.stmts[*item] {
      if binding.name == *name {
        return Some(HirDefinition::new_local(binding, block, HirDefinitionId::Stmt(*item)));
      }
    }
  }

  for (param, binding) in ast.params.iter() {
    if binding.name == *name {
      return Some(HirDefinition::new_param(binding, block, HirDefinitionId::Param(param)));
    }
  }

  for (import_id, import) in ast.imports.iter() {
    let matches = match import.rename {
      Some(ref n) => n.as_str() == name,
      None => import.path.elems.last().unwrap().as_str() == name,
    };

    if matches {
      return Some(HirDefinition {
        name:     import.path.elems.last().unwrap().clone(),
        id:       HirDefinitionId::Import(import_id),
        block_id: block,
        kind:     HirDefinitionKind::Import,
      });
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
    let matches = match import.rename {
      Some(ref n) => n.as_str() == name,
      None => import.path.elems.last().unwrap().as_str() == name,
    };

    if matches {
      return Some(import.path.clone());
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
  // FIXME: Do all this without depending on the CST directly.
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
