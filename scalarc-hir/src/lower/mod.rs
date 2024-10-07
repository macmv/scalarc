// A high level, unresolved AST of a source file. These syntax trees don't store
// any spans, and are primarily used for inferrence.

use std::sync::Arc;

use crate::{
  hir::{
    AstIdMap, Binding, BindingKind, Block, BlockId, BlockSourceMap, Expr, ExprId, Import, Literal,
    Pattern, PatternId, Stmt, StmtId, Type, UnresolvedPath,
  },
  HirDatabase, InFile, InFileExt, Name, Path, Signature,
};
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
        ast::BlockExpr(it) => break BlockId::BlockExpr(ast_id_map.ast_id(&it)),
        ast::CaseItem(it) => break BlockId::CaseItem(ast_id_map.ast_id(&it)),
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

  let block_id = block.id;

  let mut block = Block::empty();
  let mut source_map = BlockSourceMap::empty();
  let mut lower =
    BlockLower { id_map: &item_id_map, block: &mut block, source_map: &mut source_map };

  match block_id {
    BlockId::BlockExpr(block) => {
      let block = item_id_map.get(&ast, block);
      lower.walk_items(block.items());
    }

    BlockId::CaseItem(case) => {
      let case = item_id_map.get(&ast, case);

      if let Some(pat) = case.pattern() {
        lower.walk_pattern_params(&pat);
      }

      if let Some(block) = case.block() {
        lower.walk_items(block.items());
      }
    }

    BlockId::Def(def) => {
      let def = item_id_map.get(&ast, def);

      // Defs are a bit special: they only contain a single expression, but we still
      // need to track it, to go from the CST to the HIR expr. Additionally,
      // defs are the only block with params, so we walk those here.
      lower.walk(&def.expr().unwrap());
      if let Some(sig) = def.fun_sig() {
        for params in sig.fun_paramss() {
          lower.walk_params(params);
        }
      }
    }

    BlockId::Class(class) => {
      let def = item_id_map.get(&ast, class);

      if let Some(body) = def.body() {
        lower.walk_items(body.items());
      }
      if let Some(params) = def.fun_params() {
        lower.walk_params(params);
      }
    }

    BlockId::Trait(class) => {
      let def = item_id_map.get(&ast, class);

      if let Some(body) = def.body() {
        lower.walk_items(body.items());
      }
    }

    BlockId::Object(class) => {
      let def = item_id_map.get(&ast, class);

      if let Some(body) = def.body() {
        lower.walk_items(body.items());
      }
    }

    BlockId::Source(source) => {
      let item = item_id_map.get(&ast, source);

      lower.walk_items(item.items());
    }
  }

  (Arc::new(block), Arc::new(source_map))
}

impl Block {
  pub fn empty() -> Self {
    Block {
      stmts:    Arena::new(),
      exprs:    Arena::new(),
      patterns: Arena::new(),
      params:   Arena::new(),
      imports:  Arena::new(),
      items:    vec![],
    }
  }
}

/// Lowers an AST block into an HIR block.
struct BlockLower<'a> {
  block:      &'a mut Block,
  source_map: &'a mut BlockSourceMap,

  id_map: &'a AstIdMap,
}

trait Lower {
  type Output;

  fn lower(&self, lower: &mut BlockLower) -> Option<Self::Output>;
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
  fn alloc_pattern(&mut self, hir: Pattern, ast: &ast::Pattern) -> PatternId {
    let id = self.block.patterns.alloc(hir);

    self.source_map.pattern.insert(AstPtr::new(ast), id);
    self.source_map.pattern_back.insert(id, AstPtr::new(ast));

    id
  }

  fn walk_items(&mut self, items: impl Iterator<Item = ast::Item>) {
    for item in items {
      if let Some(id) = self.walk(&item) {
        self.block.items.push(id);
      }
    }
  }

  fn walk<T: Lower>(&mut self, item: &T) -> Option<T::Output> { item.lower(self) }
}

impl Lower for ast::Item {
  type Output = StmtId;

  fn lower(&self, lower: &mut BlockLower) -> Option<StmtId> {
    match self {
      ast::Item::ExprItem(expr) => {
        let expr_id = lower.walk(&expr.expr()?)?;
        let stmt_id = lower.alloc_stmt(Stmt::Expr(expr_id), self);

        Some(stmt_id)
      }

      ast::Item::FunDef(def) => {
        let sig = def.fun_sig()?;
        let name = sig.id_token()?.text().to_string();
        let sig = Signature::from_ast(&sig);
        let expr_id = match def.expr() {
          Some(e) => lower.walk(&e),
          None => None,
        };

        let stmt_id = lower.alloc_stmt(
          Stmt::Binding(Binding {
            implicit: false,
            kind: BindingKind::Def(sig),
            name,
            ty: None,
            expr: expr_id,
          }),
          self,
        );

        Some(stmt_id)
      }

      // FIXME: Re-implement pattern vals.
      /*
      ast::Item::ValDef(def) => match def.pattern()? {
        ast::Pattern::PathPattern(path) => {
          let name = path.path()?.ids().next().unwrap().text().to_string();
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

          Some(stmt_id)
        }
        ast::Pattern::TypePattern(p) => {
          let name = p.id_token()?.text().to_string();
          let expr_id = match def.expr() {
            Some(e) => Some(self.walk_expr(&e)?),
            None => None,
          };
          let ty = self.lower_type(p.ty());

          let stmt_id = self.alloc_stmt(
            Stmt::Binding(Binding {
              implicit: false,
              kind: BindingKind::Val,
              name,
              ty,
              expr: expr_id,
            }),
            item,
          );

          Some(stmt_id)
        }

        // TODO: Produce multiple bindings for cases like `val (x, y) = (1, 2)`.
        _ => None,
      },
      */
      ast::Item::ValDef(def) => {
        let name = def.id_token()?.text().to_string();
        let expr_id = match def.expr() {
          Some(e) => Some(lower.walk(&e)?),
          None => None,
        };
        let ty = lower.lower_type(def.ty());

        let stmt_id = lower.alloc_stmt(
          Stmt::Binding(Binding {
            implicit: false,
            kind: BindingKind::Val,
            name,
            ty,
            expr: expr_id,
          }),
          self,
        );

        Some(stmt_id)
      }

      ast::Item::Import(i) => {
        for import_expr in i.import_exprs() {
          match import_expr {
            ast::ImportExpr::Path(p) => {
              let mut path = Path::new();

              for id in p.ids() {
                path.elems.push(Name::new(id.text().to_string()));
              }

              lower.block.imports.alloc(Import { path, rename: None });
            }
            ast::ImportExpr::ImportSelectors(p) => {
              let mut path = Path::new();

              if let Some(p) = p.path() {
                for id in p.ids() {
                  path.elems.push(Name::new(id.text().to_string()));
                }
              }

              for sel in p.import_selectors() {
                let mut path = path.clone();
                match sel {
                  ast::ImportSelector::ImportSelectorId(sel) => {
                    let id = sel.id_token()?.text().to_string();
                    path.elems.push(Name::new(id));
                    lower.block.imports.alloc(Import { path, rename: None });
                  }
                  ast::ImportSelector::ImportSelectorRename(sel) => {
                    let from = sel.from()?.text().to_string();
                    let to = sel.to()?.text().to_string();
                    path.elems.push(Name::new(from));
                    lower.block.imports.alloc(Import { path, rename: Some(Name::new(to)) });
                  }
                  // FIXME: Import all in package is hard.
                  _ => {}
                }
              }
            }
          }
        }

        None
      }

      _ => {
        warn!("Unhandled item: {:#?}", self);
        None
      }
    }
  }
}

impl Lower for ast::Expr {
  type Output = ExprId;

  fn lower(&self, lower: &mut BlockLower) -> Option<ExprId> {
    match self {
      ast::Expr::BlockExpr(block) => {
        let id = lower.id_map.ast_id(block).into();

        Some(lower.alloc_expr(Expr::Block(id), self))
      }

      ast::Expr::IdentExpr(ident) => {
        let ident = ident.id_token()?.text().to_string();

        Some(lower.alloc_expr(Expr::Name(UnresolvedPath { segments: vec![ident] }), self))
      }

      ast::Expr::LitExpr(lit) => {
        if let Some(int) = lit.int_lit_token() {
          Some(lower.alloc_expr(Expr::Literal(Literal::Int(int.text().parse().unwrap())), self))
        } else {
          None
        }
      }

      ast::Expr::DoubleQuotedString(string) => {
        let string = string.syntax().text().to_string();

        Some(lower.alloc_expr(Expr::Literal(Literal::String(string)), self))
      }

      ast::Expr::InfixExpr(infix) => {
        let lhs = lower.walk(&infix.lhs()?)?;
        let name = infix.id_token()?.text().to_string();
        let rhs = lower.walk(&infix.rhs()?)?;

        // FIXME: This shouldn't be in the source map? Need to think through if this is
        // allowed.
        let lhs_wrapped = lower.alloc_expr(Expr::FieldAccess(lhs, name.clone()), self);
        Some(lower.alloc_expr(Expr::Call(lhs_wrapped, vec![rhs]), self))
      }

      ast::Expr::FieldExpr(field) => {
        let lhs = lower.walk(&field.expr()?)?;
        // If there is no ID token, we still act like there's a field access, so that
        // completing `foo.|` works when nothing has been typed yet.
        let name = match field.id_token() {
          Some(t) => t.text().to_string(),
          None => String::new(),
        };

        Some(lower.alloc_expr(Expr::FieldAccess(lhs, name), self))
      }

      ast::Expr::CallExpr(call) => {
        let lhs = lower.walk(&call.expr()?)?;

        let mut res = vec![];
        match call.arguments()? {
          ast::Arguments::ParenArguments(a) => {
            for arg in a.exprs() {
              res.push(lower.walk(&arg)?);
            }
          }
          _ => {}
        }

        Some(lower.alloc_expr(Expr::Call(lhs, res), self))
      }

      ast::Expr::TupleExpr(tup) => {
        let mut items = vec![];

        for expr in tup.exprs() {
          items.push(lower.walk(&expr)?);
        }

        if items.len() == 1 {
          let id = items[0];

          // This makes looking up the tuple easier. Since its the same expression, we
          // don't actually give it a unique expr ID though.
          lower.source_map.expr.insert(AstPtr::new(self), id);

          Some(id)
        } else {
          Some(lower.alloc_expr(Expr::Tuple(items), self))
        }
      }

      ast::Expr::NewExpr(new) => {
        let name = UnresolvedPath { segments: vec![new.id_token()?.text().to_string()] };
        let mut args = vec![];
        if let Some(a) = new.paren_arguments() {
          for arg in a.exprs() {
            args.push(lower.walk(&arg)?);
          }
        }

        Some(lower.alloc_expr(Expr::New(name, args), self))
      }

      ast::Expr::IfExpr(if_expr) => {
        let cond = lower.walk(&if_expr.cond()?)?;
        let then = lower.walk(&if_expr.then()?)?;
        let els = if_expr.els().and_then(|e| lower.walk(&e));

        Some(lower.alloc_expr(Expr::If(cond, then, els), self))
      }

      ast::Expr::MatchExpr(match_expr) => {
        let lhs = lower.walk(&match_expr.expr()?)?;

        let mut cases = vec![];

        for case in match_expr.case_items() {
          let pattern = lower.walk_pattern(&case.pattern()?)?;
          let block = lower.block.exprs.alloc(Expr::Block(lower.id_map.ast_id(&case).into()));

          cases.push((pattern, block));
        }

        Some(lower.alloc_expr(Expr::Match(lhs, cases), self))
      }

      _ => {
        warn!("Unhandled expr: {self:#?}");
        None
      }
    }
  }
}

impl BlockLower<'_> {
  fn walk_params(&mut self, params: ast::FunParams) {
    for param in params.fun_params() {
      let name = param.id_token().unwrap().text().to_string();

      let param_id = self.block.params.alloc(Binding {
        implicit: false,
        kind: BindingKind::Val,
        name,
        ty: self.lower_type(param.ty()),
        expr: None,
      });
      self.source_map.param.insert(SyntaxNodePtr::new(param.syntax()), param_id);
      self.source_map.param_back.insert(param_id, SyntaxNodePtr::new(param.syntax()));
    }
  }

  fn lower_type(&self, te: Option<ast::Type>) -> Option<Type> {
    let path = match te? {
      ast::Type::PathType(p) => {
        let mut path = UnresolvedPath { segments: vec![] };

        if let Some(id) = p.id_token() {
          path.segments.push(id.text().to_string());
        }

        path
      }

      ast::Type::SimpleType(p) => {
        UnresolvedPath { segments: vec![p.id_token()?.text().to_string()] }
      }

      _ => return None,
    };

    Some(Type::Named(path))
  }

  fn walk_pattern(&mut self, pat: &scalarc_syntax::ast::Pattern) -> Option<PatternId> {
    match pat {
      ast::Pattern::PathPattern(path) => {
        let path = path.path()?;

        if path.ids().count() == 1 && path.ids().next().unwrap().text() == "_" {
          Some(self.alloc_pattern(Pattern::Wildcard, pat))
        } else if path.ids().count() == 1 {
          Some(self.alloc_pattern(
            Pattern::Binding(Binding {
              implicit: false,
              kind:     BindingKind::Pattern,
              ty:       None,
              name:     path.ids().next().unwrap().text().to_string(),
              expr:     None,
            }),
            pat,
          ))
        } else {
          None
        }
      }

      _ => {
        warn!("Unhandled pattern: {:#?}", pat);
        None
      }
    }
  }

  fn walk_pattern_params(&mut self, pat: &scalarc_syntax::ast::Pattern) {
    match pat {
      ast::Pattern::PathPattern(path) => {
        let Some(path) = path.path() else { return };

        if path.ids().count() == 1 && path.ids().next().unwrap().text() == "_" {
          // TODO
        } else if path.ids().count() == 1 {
          let pattern_id = self.block.params.alloc(Binding {
            implicit: false,
            kind:     BindingKind::Pattern,
            name:     path.ids().next().unwrap().text().to_string(),
            ty:       None,
            expr:     None,
          });
          self.source_map.param.insert(SyntaxNodePtr::new(pat.syntax()), pattern_id);
          self.source_map.param_back.insert(pattern_id, SyntaxNodePtr::new(pat.syntax()));
        }
      }

      _ => {
        warn!("Unhandled pattern: {:#?}", pat);
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use scalarc_source::FileId;
  use scalarc_test::{expect, Expect};

  use crate::hir::AstId;

  use super::*;

  fn check(src: &str, expect: Expect) {
    let db = crate::tests::new_db(src);

    let file_id = FileId::temp_new();

    let (ast, _source_map) = db
      .hir_ast_with_source_for_block(InFile { file_id, id: BlockId::BlockExpr(AstId::temp_new()) });

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
                BlockExpr(
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
          patterns: Arena {
            len: 0,
            data: [],
          },
          params: Arena {
            len: 0,
            data: [],
          },
          imports: Arena {
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
          patterns: Arena {
            len: 0,
            data: [],
          },
          params: Arena {
            len: 0,
            data: [],
          },
          imports: Arena {
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
