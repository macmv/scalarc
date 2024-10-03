use scalarc_syntax::SyntaxNodePtr;

use super::{BlockId, ExprId};
use crate::{
  hir, HirDatabase, HirDefinition, HirDefinitionId, HirDefinitionKind, InFile, InFileExt,
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
        return Some(HirDefinition::from_binding(binding, block, HirDefinitionId::Stmt(*item)));
      }
    }
  }

  for (param, binding) in ast.params.iter() {
    if binding.name == *name {
      return Some(HirDefinition::from_binding(binding, block, HirDefinitionId::Param(param)));
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
