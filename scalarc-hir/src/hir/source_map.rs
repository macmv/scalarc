//! Maps HIR exprs in a Block back to source.

use super::{ExprId, StmtId};
use hashbrown::HashMap;
use scalarc_syntax::{ast, AstPtr};

#[derive(Debug, PartialEq, Eq)]
pub struct BlockSourceMap {
  // TODO: These could be a bit more efficient.
  pub(super) expr:      HashMap<AstPtr<ast::Expr>, ExprId>,
  pub(super) expr_back: HashMap<ExprId, AstPtr<ast::Expr>>,

  pub(super) stmt:      HashMap<AstPtr<ast::Item>, StmtId>,
  pub(super) stmt_back: HashMap<StmtId, AstPtr<ast::Item>>,
}

impl BlockSourceMap {
  pub fn empty() -> BlockSourceMap {
    BlockSourceMap {
      expr:      HashMap::new(),
      expr_back: HashMap::new(),
      stmt:      HashMap::new(),
      stmt_back: HashMap::new(),
    }
  }

  pub fn expr(&self, expr: AstPtr<ast::Expr>) -> Option<ExprId> { self.expr.get(&expr).copied() }
  pub fn expr_syntax(&self, id: ExprId) -> Option<AstPtr<ast::Expr>> {
    self.expr_back.get(&id).copied()
  }

  pub fn stmt(&self, stmt: AstPtr<ast::Item>) -> Option<StmtId> { self.stmt.get(&stmt).copied() }
  pub fn stmt_syntax(&self, id: StmtId) -> Option<AstPtr<ast::Item>> {
    self.stmt_back.get(&id).copied()
  }
}
