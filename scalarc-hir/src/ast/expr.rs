// A high level, unresolved AST of a source file. These syntax trees don't store
// any spans, and are primarily used for inferrence.

use std::sync::Arc;

use super::{AstId, AstIdMap, BlockSourceMap, ErasedAstId};
use crate::HirDatabase;
use hashbrown::HashMap;
use la_arena::{Arena, Idx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode, SyntaxKind},
  AstPtr,
};

pub type StmtId = Idx<Stmt>;
pub type ExprId = Idx<Expr>;

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
  pub stmts: Arena<Stmt>,
  pub exprs: Arena<Expr>,

  // After parsing to a `Block`, we need a way to lookup an AST item under the cursor, and convert
  // it to a statement. This map is used for that.
  pub stmt_map: HashMap<ErasedAstId, StmtId>,

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
  Def,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
  // Nested blocks go through HirDatabase.
  Block(AstId<scalarc_syntax::ast::BlockExpr>),

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

pub fn hir_ast_with_source_for_scope(
  db: &dyn HirDatabase,
  file_id: FileId,
  scope: Option<AstId<scalarc_syntax::ast::BlockExpr>>,
) -> (Arc<Block>, Arc<BlockSourceMap>) {
  let ast = db.parse(file_id);
  let item_id_map = db.ast_id_map(file_id);

  let (block, source_map) = match scope {
    Some(scope) => {
      let ptr = item_id_map.get_erased(scope.erased());
      let node = ptr.to_node(ast.tree().syntax());

      match node.kind() {
        SyntaxKind::BLOCK_EXPR => {
          let block = scalarc_syntax::ast::BlockExpr::cast(node).unwrap();
          ast_for_block(&item_id_map, block.items())
        }

        SyntaxKind::CLASS_DEF => {
          let def = scalarc_syntax::ast::ClassDef::cast(node).unwrap();

          if let Some(body) = def.body() {
            ast_for_block(&item_id_map, body.items())
          } else {
            (Block::empty(), BlockSourceMap::empty())
          }
        }

        _ => (Block::empty(), BlockSourceMap::empty()),
      }
    }
    None => {
      let item = ast::SourceFile::cast(ast.tree().syntax().clone()).unwrap();

      ast_for_block(&item_id_map, item.items())
    }
  };

  (Arc::new(block), Arc::new(source_map))
}

impl Block {
  pub fn empty() -> Self {
    Block {
      stmts:    Arena::new(),
      exprs:    Arena::new(),
      stmt_map: HashMap::new(),
      items:    vec![],
    }
  }

  pub fn item_id_for_ast_id(&self, id: ErasedAstId) -> Option<StmtId> {
    self.stmt_map.get(&id).copied()
  }
  pub fn item_for_ast_id(&self, id: ErasedAstId) -> Option<&Stmt> {
    Some(&self.stmts[self.item_id_for_ast_id(id)?])
  }
}

struct BlockBuilder<'a> {
  block:      &'a mut Block,
  source_map: &'a mut BlockSourceMap,

  id_map: &'a AstIdMap,
}

fn ast_for_block(
  id_map: &AstIdMap,
  items: impl Iterator<Item = ast::Item>,
) -> (Block, BlockSourceMap) {
  let mut block = Block::empty();
  let mut source_map = BlockSourceMap::empty();

  BlockBuilder { id_map, block: &mut block, source_map: &mut source_map }.walk_items(items);

  (block, source_map)
}

impl BlockBuilder<'_> {
  fn alloc_expr(&mut self, hir: Expr, ast: &ast::Expr) -> ExprId {
    let id = self.block.exprs.alloc(hir);

    self.source_map.expr.insert(AstPtr::new(ast), id);
    self.source_map.expr_back.insert(id, AstPtr::new(ast));

    id
  }
  fn alloc_stmt(&mut self, hir: Stmt, ast: &ast::Item) -> StmtId {
    let id = self.block.stmts.alloc(hir);

    self.source_map.stmt.insert(AstPtr::new(ast), id);
    self.source_map.stmt_back.insert(id, AstPtr::new(ast));

    id
  }

  fn walk_items(&mut self, items: impl Iterator<Item = ast::Item>) {
    for item in items {
      if let Some(id) = self.walk_stmt(&item) {
        self.block.items.push(id);
      }
    }
  }

  fn walk_stmt(&mut self, item: &scalarc_syntax::ast::Item) -> Option<StmtId> {
    match item {
      scalarc_syntax::ast::Item::ExprItem(expr) => {
        let expr_id = self.walk_expr(&expr.expr()?)?;
        let stmt_id = self.alloc_stmt(Stmt::Expr(expr_id), item);

        Some(stmt_id)
      }

      scalarc_syntax::ast::Item::FunDef(def) => {
        let sig = def.fun_sig()?;
        let name = sig.id_token()?.text().to_string();
        let expr_id = self.walk_expr(&def.expr()?)?;

        let stmt_id = self.alloc_stmt(
          Stmt::Binding(Binding {
            implicit: false,
            kind: BindingKind::Def,
            name,
            ty: None,
            expr: expr_id,
          }),
          item,
        );

        self.block.stmt_map.insert(self.id_map.ast_id(def).erased(), stmt_id);
        Some(stmt_id)
      }

      scalarc_syntax::ast::Item::ValDef(def) => {
        let name = def.id_token()?.text().to_string();
        let expr_id = self.walk_expr(&def.expr()?)?;

        let stmt_id = self.alloc_stmt(
          Stmt::Binding(Binding {
            implicit: false,
            kind: BindingKind::Val,
            name,
            ty: None,
            expr: expr_id,
          }),
          item,
        );

        self.block.stmt_map.insert(self.id_map.ast_id(def).erased(), stmt_id);
        Some(stmt_id)
      }

      _ => {
        warn!("Unhandled item: {:#?}", item);
        None
      }
    }
  }

  fn walk_expr(&mut self, expr: &scalarc_syntax::ast::Expr) -> Option<ExprId> {
    match expr {
      scalarc_syntax::ast::Expr::BlockExpr(block) => {
        let id = self.id_map.ast_id(block);

        Some(self.alloc_expr(Expr::Block(id), expr))
      }

      scalarc_syntax::ast::Expr::IdentExpr(ident) => {
        let ident = ident.id_token()?.text().to_string();

        Some(self.alloc_expr(Expr::Name(UnresolvedPath { segments: vec![ident] }), expr))
      }

      scalarc_syntax::ast::Expr::LitExpr(lit) => {
        if let Some(int) = lit.int_lit_token() {
          Some(self.alloc_expr(Expr::Literal(Literal::Int(int.text().parse().unwrap())), expr))
        } else {
          None
        }
      }

      scalarc_syntax::ast::Expr::InfixExpr(infix) => {
        let lhs = self.walk_expr(&infix.lhs()?)?;
        let name = infix.id_token()?.text().to_string();
        let rhs = self.walk_expr(&infix.rhs()?)?;

        Some(self.alloc_expr(Expr::Call(lhs, name, vec![rhs]), expr))
      }

      scalarc_syntax::ast::Expr::FieldExpr(field) => {
        let lhs = self.walk_expr(&field.expr()?)?;
        let name = field.id_token()?.text().to_string();

        Some(self.alloc_expr(Expr::FieldAccess(lhs, name), expr))
      }

      _ => {
        warn!("Unhandled expr: {expr:#?}");
        None
      }
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

    let (ast, _source_map) = db.hir_ast_with_source_for_scope(
      file_id,
      Some(AstId {
        raw:     Idx::from_raw(RawIdx::from_u32(2)),
        phantom: std::marker::PhantomData,
      }),
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
        val bar = {
          val x = 3
          x
        }
        foo
      }
      "#,
      expect![@r#"
        Block {
          stmts: Arena {
            len: 5,
            data: [
              Expr(
                Idx::<Expr>(0),
              ),
              Expr(
                Idx::<Expr>(3),
              ),
              Binding(
                Binding {
                  implicit: false,
                  kind: Val,
                  ty: None,
                  name: "foo",
                  expr: Idx::<Expr>(6),
                },
              ),
              Binding(
                Binding {
                  implicit: false,
                  kind: Val,
                  ty: None,
                  name: "bar",
                  expr: Idx::<Expr>(7),
                },
              ),
              Expr(
                Idx::<Expr>(8),
              ),
            ],
          },
          exprs: Arena {
            len: 9,
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
              Literal(
                Int(
                  4,
                ),
              ),
              Literal(
                Int(
                  5,
                ),
              ),
              Call(
                Idx::<Expr>(4),
                "+",
                [
                  Idx::<Expr>(5),
                ],
              ),
              Block(
                AstId {
                  raw: Idx::<Scala>>(8),
                  phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::BlockExpr>,
                },
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
          stmt_map: {
            ErasedAstId {
              raw: Idx::<Scala>>(6),
            }: Idx::<Stmt>(3),
            ErasedAstId {
              raw: Idx::<Scala>>(5),
            }: Idx::<Stmt>(2),
          },
          items: [
            Idx::<Stmt>(0),
            Idx::<Stmt>(1),
            Idx::<Stmt>(2),
            Idx::<Stmt>(3),
            Idx::<Stmt>(4),
          ],
        }"#
      ],
    );
  }
}
