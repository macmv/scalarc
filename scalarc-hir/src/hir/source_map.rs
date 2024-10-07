//! Maps HIR exprs in a Block back to source.

use super::{ExprId, ParamId, PatternId, StmtId};
use hashbrown::HashMap;
use scalarc_syntax::{ast, AstPtr, SyntaxNodePtr};

#[derive(Debug, PartialEq, Eq)]
pub struct BlockSourceMap {
  // TODO: These could be a bit more efficient.
  pub(crate) expr:      HashMap<AstPtr<ast::Expr>, ExprId>,
  pub(crate) expr_back: HashMap<ExprId, AstPtr<ast::Expr>>,

  pub(crate) stmt:      HashMap<AstPtr<ast::Item>, StmtId>,
  pub(crate) stmt_back: HashMap<StmtId, AstPtr<ast::Item>>,

  pub(crate) pattern:      HashMap<AstPtr<ast::Pattern>, PatternId>,
  pub(crate) pattern_back: HashMap<PatternId, AstPtr<ast::Pattern>>,

  pub(crate) param:      HashMap<SyntaxNodePtr, ParamId>,
  pub(crate) param_back: HashMap<ParamId, SyntaxNodePtr>,
}

impl BlockSourceMap {
  pub fn empty() -> BlockSourceMap {
    BlockSourceMap {
      expr:         HashMap::new(),
      expr_back:    HashMap::new(),
      stmt:         HashMap::new(),
      stmt_back:    HashMap::new(),
      pattern:      HashMap::new(),
      pattern_back: HashMap::new(),
      param:        HashMap::new(),
      param_back:   HashMap::new(),
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

  pub fn pattern(&self, stmt: AstPtr<ast::Pattern>) -> Option<PatternId> {
    self.pattern.get(&stmt).copied()
  }
  pub fn pattern_syntax(&self, id: PatternId) -> Option<AstPtr<ast::Pattern>> {
    self.pattern_back.get(&id).copied()
  }

  pub fn param(&self, param: SyntaxNodePtr) -> Option<ParamId> { self.param.get(&param).copied() }
  pub fn param_syntax(&self, id: ParamId) -> Option<SyntaxNodePtr> {
    self.param_back.get(&id).copied()
  }
}
