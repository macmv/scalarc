// A high level, unresolved AST of a source file. These syntax trees don't store
// any spans, and are primarily used for inferrence.

use std::sync::Arc;

use super::AstId;
use crate::HirDatabase;
use la_arena::{Arena, Idx};
use scalarc_source::FileId;

pub type StmtId = Idx<Stmt>;
pub type ExprId = Idx<Expr>;

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
  pub stmts: Arena<Stmt>,
  pub exprs: Arena<Expr>,

  pub items: Vec<StmtId>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
  Named(UnresolvedPath),
  // TODO: Tuples and such?
}

/// An unresolved path to a type.
#[derive(Debug, PartialEq, Eq)]
pub struct UnresolvedPath {
  pub segments: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
  Binding(Binding),
  Expr(ExprId),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Binding {
  pub implicit: bool,
  pub kind:     BindingKind,
  pub ty:       Option<Type>,
  pub name:     String,
  pub expr:     ExprId,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BindingKind {
  Val,
  Var,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
  Literal(Literal),
  Name(UnresolvedPath),
  Underscore,
  Tuple(Vec<ExprId>),

  FieldAccess(ExprId, String),
  Call(ExprId, Vec<ExprId>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
  Int(i32),
  Long(i64),
  Float(EqFloat),
  String(String),
  Char(char),
  Bool(bool),
}

#[derive(Debug, PartialEq)]
pub struct EqFloat(f64);

impl Eq for EqFloat {}

pub fn hir_ast_for_scope(
  db: &dyn HirDatabase,
  file_id: FileId,
  scope: AstId<scalarc_syntax::ast::BlockExpr>,
) -> Arc<()> {
  let ast = db.parse(file_id);
  let item_id_map = db.item_id_map(file_id);

  let item = item_id_map.get(&ast, scope);

  // Arc::new(ast_for_block(&item))
  Arc::new(())
}

fn ast_for_block(ast: &scalarc_syntax::ast::BlockExpr) -> Block {
  let mut block = Block { stmts: Arena::new(), exprs: Arena::new(), items: vec![] };

  block.walk_block(ast);

  block
}

impl Block {
  fn walk_block(&mut self, ast: &scalarc_syntax::ast::BlockExpr) {
    for item in ast.items() {
      if let Some(id) = self.walk_stmt(&item) {
        self.items.push(id);
      }
    }
  }

  fn walk_stmt(&mut self, item: &scalarc_syntax::ast::Item) -> Option<StmtId> {
    match item {
      scalarc_syntax::ast::Item::ExprItem(expr) => {
        let expr_id = self.walk_expr(&expr.expr()?)?;
        Some(self.stmts.alloc(Stmt::Expr(expr_id)))
      }

      _ => None,
    }
  }

  fn walk_expr(&mut self, expr: &scalarc_syntax::ast::Expr) -> Option<ExprId> {
    match expr {
      scalarc_syntax::ast::Expr::LitExpr(expr) => {
        if let Some(int) = expr.int_lit_token() {
          let expr = Expr::Literal(Literal::Int(int.text().parse().unwrap()));

          Some(self.exprs.alloc(expr))
        } else {
          None
        }
      }

      _ => None,
    }
  }
}
