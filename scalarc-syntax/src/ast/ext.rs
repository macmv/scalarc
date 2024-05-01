use super::support;
use crate::ast;

impl ast::InfixExpr {
  pub fn lhs(&self) -> Option<ast::Expr> { support::children(&self.syntax).next() }
  pub fn rhs(&self) -> Option<ast::Expr> { support::children(&self.syntax).nth(1) }
}

impl ast::IfExpr {
  pub fn cond(&self) -> Option<ast::Expr> { support::children(&self.syntax).next() }
  pub fn then(&self) -> Option<ast::Expr> { support::children(&self.syntax).nth(1) }
  pub fn els(&self) -> Option<ast::Expr> { support::children(&self.syntax).nth(2) }
}
