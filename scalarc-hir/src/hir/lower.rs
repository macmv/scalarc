// A high level, unresolved AST of a source file. These syntax trees don't store
// any spans, and are primarily used for inferrence.

use std::sync::Arc;

use super::{
  AstIdMap, Binding, BindingKind, Block, BlockId, BlockSourceMap, ErasedAstId, Expr, ExprId,
  Literal, Stmt, StmtId, UnresolvedPath,
};
use crate::{HirDatabase, InFile, InFileExt, Signature};
use hashbrown::HashMap;
use la_arena::Arena;
use scalarc_syntax::{
  ast::{self, AstNode},
  AstPtr, SyntaxNodePtr,
};

pub fn block_for_node(db: &dyn HirDatabase, ptr: InFile<SyntaxNodePtr>) -> InFile<BlockId> {
  let ast = db.parse(ptr.file_id);

  let ast_id_map = db.ast_id_map(ptr.file_id);
  let mut node = ptr.id.to_node(&ast.syntax_node());

  let id = loop {
    scalarc_syntax::match_ast! {
      match node {
        ast::BlockExpr(it) => break BlockId::Block(ast_id_map.ast_id(&it)),
        ast::FunDef(it) => break BlockId::Def(ast_id_map.ast_id(&it)),
        ast::ClassDef(it) => break BlockId::Class(ast_id_map.ast_id(&it)),
        ast::TraitDef(it) => break BlockId::Trait(ast_id_map.ast_id(&it)),
        ast::ObjectDef(it) => break BlockId::Object(ast_id_map.ast_id(&it)),
        ast::SourceFile(it) => break BlockId::Source(ast_id_map.ast_id(&it)),
        _ => node = node.parent().unwrap(),
      }
    }
  };

  id.in_file(ptr.file_id)
}

pub fn hir_ast_with_source_for_block(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
) -> (Arc<Block>, Arc<BlockSourceMap>) {
  let ast = db.parse(block.file_id);
  let item_id_map = db.ast_id_map(block.file_id);

  let (block, source_map) = match block.id {
    BlockId::Block(block) => {
      let block = item_id_map.get(&ast, block);

      ast_for_block(&item_id_map, block.items())
    }

    BlockId::Def(def) => {
      let def = item_id_map.get(&ast, def);

      let mut block = Block::empty();
      let mut source_map = BlockSourceMap::empty();
      let mut lower = BlockLower {
        id_map:     &item_id_map,
        block:      &mut block,
        source_map: &mut source_map,
      };

      // Defs are a bit special: they only contain a single expression, but we still
      // need to track it, to go from the CST to the HIR expr. Additionally,
      // defs are the only block with params, so we walk those here.
      lower.walk_expr(&def.expr().unwrap());
      lower.walk_params(def);

      (block, source_map)
    }

    BlockId::Class(class) => {
      let def = item_id_map.get(&ast, class);

      if let Some(body) = def.body() {
        ast_for_block(&item_id_map, body.items())
      } else {
        (Block::empty(), BlockSourceMap::empty())
      }
    }

    BlockId::Trait(class) => {
      let def = item_id_map.get(&ast, class);

      if let Some(body) = def.body() {
        ast_for_block(&item_id_map, body.items())
      } else {
        (Block::empty(), BlockSourceMap::empty())
      }
    }

    BlockId::Object(class) => {
      let def = item_id_map.get(&ast, class);

      if let Some(body) = def.body() {
        ast_for_block(&item_id_map, body.items())
      } else {
        (Block::empty(), BlockSourceMap::empty())
      }
    }

    BlockId::Source(source) => {
      let item = item_id_map.get(&ast, source);

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
      params:   Arena::new(),
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

/// Lowers an AST block into an HIR block.
struct BlockLower<'a> {
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

  BlockLower { id_map, block: &mut block, source_map: &mut source_map }.walk_items(items);

  (block, source_map)
}

impl BlockLower<'_> {
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
      ast::Item::ExprItem(expr) => {
        let expr_id = self.walk_expr(&expr.expr()?)?;
        let stmt_id = self.alloc_stmt(Stmt::Expr(expr_id), item);

        Some(stmt_id)
      }

      ast::Item::FunDef(def) => {
        let sig = def.fun_sig()?;
        let name = sig.id_token()?.text().to_string();
        let sig = Signature::from_ast(&sig);
        let expr_id = match def.expr() {
          Some(e) => self.walk_expr(&e),
          None => None,
        };

        let stmt_id = self.alloc_stmt(
          Stmt::Binding(Binding {
            implicit: false,
            kind: BindingKind::Def(sig),
            name,
            ty: None,
            expr: expr_id,
          }),
          item,
        );

        self.block.stmt_map.insert(self.id_map.ast_id(def).erased(), stmt_id);
        Some(stmt_id)
      }

      ast::Item::ValDef(def) => {
        let name = def.id_token()?.text().to_string();
        let expr_id = match def.expr() {
          Some(e) => Some(self.walk_expr(&e)?),
          None => None,
        };

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
      ast::Expr::BlockExpr(block) => {
        let id = self.id_map.ast_id(block).into();

        Some(self.alloc_expr(Expr::Block(id), expr))
      }

      ast::Expr::IdentExpr(ident) => {
        let ident = ident.id_token()?.text().to_string();

        Some(self.alloc_expr(Expr::Name(UnresolvedPath { segments: vec![ident] }), expr))
      }

      ast::Expr::LitExpr(lit) => lit
        .int_lit_token()
        .map(|int| self.alloc_expr(Expr::Literal(Literal::Int(int.text().parse().unwrap())), expr)),

      ast::Expr::InfixExpr(infix) => {
        let lhs = self.walk_expr(&infix.lhs()?)?;
        let name = infix.id_token()?.text().to_string();
        let rhs = self.walk_expr(&infix.rhs()?)?;

        // FIXME: This shouldn't be in the source map? Need to think through if this is
        // allowed.
        let lhs_wrapped = self.alloc_expr(Expr::FieldAccess(lhs, name.clone()), expr);
        Some(self.alloc_expr(Expr::Call(lhs_wrapped, vec![rhs]), expr))
      }

      ast::Expr::FieldExpr(field) => {
        let lhs = self.walk_expr(&field.expr()?)?;
        // If there is no ID token, we still act like there's a field access, so that
        // completing `foo.|` works when nothing has been typed yet.
        let name = match field.id_token() {
          Some(t) => t.text().to_string(),
          None => String::new(),
        };

        Some(self.alloc_expr(Expr::FieldAccess(lhs, name), expr))
      }

      ast::Expr::CallExpr(call) => {
        let lhs = self.walk_expr(&call.expr()?)?;

        let mut res = vec![];
        match call.arguments()? {
          ast::Arguments::ParenArguments(a) => {
            for arg in a.exprs() {
              res.push(self.walk_expr(&arg)?);
            }
          }
          _ => {}
        }

        Some(self.alloc_expr(Expr::Call(lhs, res), expr))
      }

      ast::Expr::TupleExpr(tup) => {
        let mut items = vec![];

        for expr in tup.exprs() {
          items.push(self.walk_expr(&expr)?);
        }

        if items.len() == 1 {
          let id = items[0];

          // This makes looking up the tuple easier. Since its the same expression, we
          // don't actually give it a unique expr ID though.
          self.source_map.expr.insert(AstPtr::new(expr), id);

          Some(id)
        } else {
          Some(self.alloc_expr(Expr::Tuple(items), expr))
        }
      }

      ast::Expr::NewExpr(new) => {
        let name = UnresolvedPath { segments: vec![new.id_token()?.text().to_string()] };
        let mut args = vec![];
        if let Some(a) = new.paren_arguments() {
          for arg in a.exprs() {
            args.push(self.walk_expr(&arg)?);
          }
        }

        Some(self.alloc_expr(Expr::New(name, args), expr))
      }

      _ => {
        warn!("Unhandled expr: {expr:#?}");
        None
      }
    }
  }

  fn walk_params(&mut self, def: ast::FunDef) {
    if let Some(sig) = def.fun_sig() {
      for params in sig.fun_paramss() {
        for param in params.fun_params() {
          let name = param.id_token().unwrap().text().to_string();

          let param_id = self.block.params.alloc(Binding {
            implicit: false,
            kind: BindingKind::Val,
            name,
            ty: None,
            expr: None,
          });
          self.source_map.param.insert(AstPtr::new(&param), param_id);
          self.source_map.param_back.insert(param_id, AstPtr::new(&param));
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use la_arena::{Idx, RawIdx};
  use scalarc_source::FileId;
  use scalarc_test::{expect, Expect};

  use crate::hir::AstId;

  use super::*;

  fn check(src: &str, expect: Expect) {
    let db = crate::tests::new_db(src);

    let file_id = FileId::temp_new();

    let (ast, _source_map) = db.hir_ast_with_source_for_block(InFile {
      file_id,
      id: BlockId::Block(AstId {
        raw:     Idx::from_raw(RawIdx::from_u32(1)),
        phantom: std::marker::PhantomData,
      }),
    });

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
                Idx::<Expr>(4),
              ),
              Binding(
                Binding {
                  implicit: false,
                  kind: Val,
                  ty: None,
                  name: "foo",
                  expr: Some(
                    Idx::<Expr>(8),
                  ),
                },
              ),
              Binding(
                Binding {
                  implicit: false,
                  kind: Val,
                  ty: None,
                  name: "bar",
                  expr: Some(
                    Idx::<Expr>(9),
                  ),
                },
              ),
              Expr(
                Idx::<Expr>(10),
              ),
            ],
          },
          exprs: Arena {
            len: 11,
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
              FieldAccess(
                Idx::<Expr>(1),
                "+",
              ),
              Call(
                Idx::<Expr>(3),
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
              FieldAccess(
                Idx::<Expr>(5),
                "+",
              ),
              Call(
                Idx::<Expr>(7),
                [
                  Idx::<Expr>(6),
                ],
              ),
              Block(
                Block(
                  AstId {
                    raw: Idx::<Scala>>(4),
                    phantom: PhantomData<fn() -> scalarc_syntax::ast::generated::nodes::BlockExpr>,
                  },
                ),
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
              raw: Idx::<Scala>>(3),
            }: Idx::<Stmt>(3),
            ErasedAstId {
              raw: Idx::<Scala>>(2),
            }: Idx::<Stmt>(2),
          },
          params: Arena {
            len: 0,
            data: [],
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

  #[test]
  fn new_expr() {
    check(
      r#"
      {
        val foo = new Foo(2, 3)
      }
      "#,
      expect![@r#"
        Block {
          stmts: Arena {
            len: 1,
            data: [
              Binding(
                Binding {
                  implicit: false,
                  kind: Val,
                  ty: None,
                  name: "foo",
                  expr: Some(
                    Idx::<Expr>(2),
                  ),
                },
              ),
            ],
          },
          exprs: Arena {
            len: 3,
            data: [
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
              New(
                UnresolvedPath {
                  segments: [
                    "Foo",
                  ],
                },
                [
                  Idx::<Expr>(0),
                  Idx::<Expr>(1),
                ],
              ),
            ],
          },
          stmt_map: {
            ErasedAstId {
              raw: Idx::<Scala>>(2),
            }: Idx::<Stmt>(0),
          },
          params: Arena {
            len: 0,
            data: [],
          },
          items: [
            Idx::<Stmt>(0),
          ],
        }"#
      ],
    );
  }
}
