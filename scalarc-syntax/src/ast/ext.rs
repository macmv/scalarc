use super::support;
use crate::ast;

impl ast::InfixExpr {
  pub fn lhs(&self) -> Option<ast::Expr> { support::children(&self.syntax).next() }
  pub fn rhs(&self) -> Option<ast::Expr> { support::children(&self.syntax).nth(1) }
}
