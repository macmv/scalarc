//! Lowers `scalarc_syntax::ast` into` scalarc_hir::tree`.

use crate::tree::{self, ExprId, ItemId};
use scalarc_source::SourceDatabase;
use scalarc_syntax::{
  ast::{self},
  SourceFile,
};

#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase: SourceDatabase {}

struct Lower {
  item_arena: la_arena::Arena<tree::Item>,
  expr_arena: la_arena::Arena<tree::Expr>,
}

pub fn lower(ast: SourceFile) -> tree::Block {
  let item_arena = la_arena::Arena::new();
  let expr_arena = la_arena::Arena::new();

  let mut lower = Lower { item_arena, expr_arena };
  lower.source_file(ast)
}

impl Lower {
  fn source_file(&mut self, ast: SourceFile) -> tree::Block {
    let mut items = Vec::new();
    for item in ast.items() {
      if let Some(it) = self.lower_item(item) {
        items.push(it);
      }
    }

    tree::Block { items: items.into_boxed_slice() }
  }

  fn lower_item(&mut self, item: ast::Item) -> Option<ItemId> {
    let item = match item {
      ast::Item::ValDef(e) => self.lower_val_def(e)?,
      _ => todo!("lowering for {:?}", item),
    };
    Some(self.item_arena.alloc(item))
  }

  fn lower_expr(&mut self, expr: ast::Expr) -> Option<ExprId> {
    let expr = match expr {
      ast::Expr::LitExpr(e) => self.lower_literal(e)?,
      ast::Expr::InfixExpr(e) => self.lower_infix(e)?,
      _ => todo!("lowering for {:?}", expr),
    };

    Some(self.expr_arena.alloc(expr))
  }

  fn lower_val_def(&mut self, val_def: ast::ValDef) -> Option<tree::Item> {
    let name = val_def.id_token()?.text().into();
    let expr = self.lower_expr(val_def.expr()?)?;

    Some(tree::Item::Val { name, expr })
  }

  fn lower_literal(&mut self, lit: ast::LitExpr) -> Option<tree::Expr> {
    // TODO: Fix literal ast.
    if let Some(lit) = lit.int_lit_token() {
      return Some(tree::Expr::Literal(tree::Literal::Int(lit.text().parse().unwrap())));
    }

    None
  }

  fn lower_infix(&mut self, infix: ast::InfixExpr) -> Option<tree::Expr> {
    let lhs = self.lower_expr(infix.lhs()?)?;
    let rhs = self.lower_expr(infix.rhs()?)?;

    Some(tree::Expr::InfixOp { lhs, rhs, op: infix.id_token()?.text().into() })
  }
}
