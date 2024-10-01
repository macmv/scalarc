use super::{BlockId, ExprId};
use crate::{hir, DefinitionKind, HirDatabase, InFile, LocalDefinition, Name};

pub fn def_for_expr(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  expr: ExprId,
) -> Option<LocalDefinition> {
  let ast = db.hir_ast_for_scope(block);

  match ast.exprs[expr] {
    hir::Expr::Name(ref path) => {
      // TODO: Fix this? Most of the time there's just one segment, so we're going to
      // pull that out.
      let name = path.segments.last().unwrap();

      for item in ast.items.iter() {
        if let hir::Stmt::Binding(ref binding) = ast.stmts[*item] {
          if binding.name == *name {
            return Some(LocalDefinition {
              name:     Name::new(binding.name.clone()),
              stmt_id:  *item,
              block_id: block,
              kind:     DefinitionKind::Val(None),
            });
          }
        }
      }

      None
    }

    _ => None,
  }
}
