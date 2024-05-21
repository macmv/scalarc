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
  Call(ExprId, String, Vec<ExprId>),
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
) -> Arc<Block> {
  let ast = db.parse(file_id);
  let item_id_map = db.item_id_map(file_id);

  let item = item_id_map.get(&ast, scope);

  Arc::new(ast_for_block(&item))
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
      scalarc_syntax::ast::Expr::IdentExpr(expr) => {
        let ident = expr.id_token()?.text().to_string();

        let expr = Expr::Name(UnresolvedPath { segments: vec![ident] });

        Some(self.exprs.alloc(expr))
      }

      scalarc_syntax::ast::Expr::LitExpr(expr) => {
        if let Some(int) = expr.int_lit_token() {
          let expr = Expr::Literal(Literal::Int(int.text().parse().unwrap()));

          Some(self.exprs.alloc(expr))
        } else {
          None
        }
      }

      scalarc_syntax::ast::Expr::InfixExpr(expr) => {
        let lhs = self.walk_expr(&expr.lhs()?)?;
        let name = expr.id_token()?.text().to_string();
        let rhs = self.walk_expr(&expr.rhs()?)?;

        let expr = Expr::Call(lhs, name, vec![rhs]);

        Some(self.exprs.alloc(expr))
      }

      _ => None,
    }
  }
}

#[cfg(test)]
mod tests {
  use la_arena::RawIdx;
  use scalarc_test::{expect, Expect};

  use super::*;

  fn check(src: &str, expect: Expect) {
    let db = crate::tests::new_db(src);

    let file_id = FileId::temp_new();

    let ast = db.hir_ast_for_scope(
      file_id,
      AstId { raw: Idx::from_raw(RawIdx::from_u32(2)), phantom: std::marker::PhantomData },
    );

    expect.assert_eq(&format!("{ast:#?}").replace("    ", "  "));
  }

  #[test]
  fn hir_ast() {
    check(
      r#"
      {
        2
        2 + 3
        val foo = 4 + 5
        foo
      }
      "#,
      expect![@r#"
        Block {
          stmts: Arena {
            len: 3,
            data: [
              Expr(
                Idx::<Expr>(0),
              ),
              Expr(
                Idx::<Expr>(3),
              ),
              Expr(
                Idx::<Expr>(4),
              ),
            ],
          },
          exprs: Arena {
            len: 5,
            data: [
              Literal(
                Int(
                  2,
                ),
              ),
              Literal(
                Int(
                  2,
                ),
              ),
              Literal(
                Int(
                  3,
                ),
              ),
              Call(
                Idx::<Expr>(1),
                "+",
                [
                  Idx::<Expr>(2),
                ],
              ),
              Name(
                UnresolvedPath {
                  segments: [
                    "foo",
                  ],
                },
              ),
            ],
          },
          items: [
            Idx::<Stmt>(0),
            Idx::<Stmt>(1),
            Idx::<Stmt>(2),
          ],
        }"#
      ],
    );
  }
}
