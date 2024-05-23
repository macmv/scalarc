//! Maps HIR exprs in a Block back to source.

use super::ExprId;
use hashbrown::HashMap;
use scalarc_syntax::{ast, AstPtr};

pub struct BlockSourceMap {
  // TODO: These could be a bit more efficient.
  expr:      HashMap<AstPtr<ast::Expr>, ExprId>,
  expr_back: HashMap<ExprId, AstPtr<ast::Expr>>,
}

impl BlockSourceMap {
  pub fn expr(&self, expr: AstPtr<ast::Expr>) -> Option<ExprId> { self.expr.get(&expr).copied() }

  pub fn expr_syntax(&self, id: ExprId) -> Option<AstPtr<ast::Expr>> {
    self.expr_back.get(&id).copied()
  }
}
