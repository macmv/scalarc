//! Lowers `scalarc_syntax::ast` into` scalarc_hir::tree`.

use crate::{
  source_map::SourceMap,
  tree::{self, ExprId, ItemId, Package, PackageArenas},
  HirDatabase,
};
use la_arena::{Idx, RawIdx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self},
  SourceFile,
};

struct Lower<'a> {
  db:         &'a dyn HirDatabase,
  source_map: &'a SourceMap,
  arenas:     PackageArenas,
}

pub fn lower(db: &dyn HirDatabase, file: FileId) -> Package {
  let mut lower = Lower { db, source_map: &db.source_map(file), arenas: Default::default() };
  let items = lower.package(db.parse(file).tree());
  Package { items, arenas: lower.arenas }
}

impl Lower<'_> {
  fn package(&mut self, ast: SourceFile) -> Box<[tree::Item]> {
    let mut items = Vec::new();
    for item in ast.items() {
      if let Some(it) = self.lower_item(item) {
        items.push(it);
      }
    }

    Vec::into_boxed_slice(items)
  }

  fn lower_item(&mut self, item: ast::Item) -> Option<tree::Item> {
    let item = match item {
      ast::Item::ValDef(e) => self.lower_val_def(e)?,
      ast::Item::FunDef(f) => self.lower_fun_def(f)?,
      _ => todo!("lowering for {:?}", item),
    };
    Some(item)
  }

  fn lower_expr(&mut self, expr: ast::Expr) -> Option<ExprId> {
    let expr = match expr {
      ast::Expr::LitExpr(e) => self.lower_literal(e)?,
      ast::Expr::InfixExpr(e) => self.lower_infix(e)?,
      _ => todo!("lowering for {:?}", expr),
    };

    todo!()

    // Some(self.expr_arena.alloc(expr))
  }

  fn lower_val_def(&mut self, val: ast::ValDef) -> Option<tree::Item> {
    let name = val.id_token()?.text().into();
    // let expr = self.lower_expr(val_def.expr()?)?;

    Some(tree::Item::Val(self.arenas.val.alloc(tree::Val {
      id: self.source_map.id(&val)?,
      name,
      expr: Idx::from_raw(RawIdx::from_u32(0)),
    })))
  }

  fn lower_fun_def(&mut self, f: ast::FunDef) -> Option<tree::Item> {
    let sig = f.fun_sig()?;
    let name = sig.id_token()?.text().into();

    Some(tree::Item::Def(self.arenas.def.alloc(tree::Def {
      id: self.source_map.id(&f)?,
      args: Box::new([]),
      name,
      body: Idx::from_raw(RawIdx::from_u32(0)),
    })))
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
