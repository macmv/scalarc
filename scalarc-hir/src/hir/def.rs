use scalarc_syntax::SyntaxNodePtr;

use super::{BlockId, ExprId};
use crate::{
  hir, HirDatabase, HirDefinition, HirDefinitionId, HirDefinitionKind, InFile, InFileExt, Name,
};

pub fn def_for_expr(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  expr: ExprId,
) -> Option<HirDefinition> {
  let ast = db.hir_ast_for_block(block);

  match ast.exprs[expr] {
    hir::Expr::Name(ref path) => {
      // TODO: Fix this? Most of the time there's just one segment, so we're going to
      // pull that out.
      let name = path.segments.last().unwrap();

      lookup_name_in_block(db, block, name)
    }

    _ => None,
  }
}

fn lookup_name_in_block(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  name: &str,
) -> Option<HirDefinition> {
  let ast = db.hir_ast_for_block(block);

  for item in ast.items.iter() {
    if let hir::Stmt::Binding(ref binding) = ast.stmts[*item] {
      if binding.name == *name {
        return Some(HirDefinition {
          name:     Name::new(binding.name.clone()),
          id:       HirDefinitionId::Stmt(*item),
          block_id: block,
          kind:     HirDefinitionKind::Val(None),
        });
      }
    }
  }

  for (param, binding) in ast.params.iter() {
    if binding.name == *name {
      return Some(HirDefinition {
        name:     Name::new(binding.name.clone()),
        id:       HirDefinitionId::Param(param),
        block_id: block,
        kind:     HirDefinitionKind::Val(None),
      });
    }
  }

  for (import_id, import) in ast.imports.iter() {
    if import.path.elems.last().unwrap().as_str() == name {
      return Some(HirDefinition {
        name:     import.path.elems.last().unwrap().clone(),
        id:       HirDefinitionId::Import(import_id),
        block_id: block,
        kind:     HirDefinitionKind::Import,
      });
    }
  }

  // FIXME: Do all this without depending on the CST directly.
  let ast_id = block.id.erased();
  let ast = db.parse(block.file_id);
  let ast_id_map = db.ast_id_map(block.file_id);
  let mut ptr = ast_id_map.get_erased(ast_id).clone();

  loop {
    let outer_block = db.block_for_node(ptr.in_file(block.file_id));
    if outer_block != block {
      return lookup_name_in_block(db, outer_block, name);
    }

    let node = ptr.to_node(&ast.syntax_node());
    let parent = node.parent()?;
    ptr = SyntaxNodePtr::new(&parent);
  }
}
