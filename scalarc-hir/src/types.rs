//! Type inferrence.
//!
//! This should probably be its own crate. Ah well.

use std::{collections::HashMap, fmt, sync::Arc};

use crate::{
  ast::{AstId, AstIdMap, Block, BlockId, ErasedAstId, Expr, ExprId, Literal, Stmt},
  tree::Name,
  DefinitionKind, HirDatabase, Path,
};
use la_arena::{Idx, RawIdx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode, SyntaxKind},
  node::SyntaxNode,
  AstPtr, TextSize, T,
};

#[derive(Clone, PartialEq, Eq)]
pub struct Type {
  pub path: Path,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Signature {
  pub params: Vec<Params>,
  pub ret:    Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Params {
  pub implicit: bool,
  pub params:   Vec<(Name, Type)>,
}

impl fmt::Debug for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "Type({})", self) }
}
impl fmt::Debug for Signature {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "Signature({})", self) }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, elem) in self.path.elems.iter().enumerate() {
      if i != 0 {
        write!(f, ".")?;
      }
      write!(f, "{}", elem.as_str())?;
    }

    Ok(())
  }
}

impl fmt::Display for Signature {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for params in &self.params {
      write!(f, "(")?;
      for (i, param) in params.params.iter().enumerate() {
        if i != 0 {
          write!(f, ", ")?;
        }
        write!(f, "{}: {}", param.0.as_str(), param.1)?;
      }
      write!(f, ")")?;
    }

    if let Some(ret) = &self.ret {
      write!(f, " => {}", ret)?;
    }

    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Inference {
  types: HashMap<ExprId, Type>,
}

struct Infer<'a> {
  db:      &'a dyn HirDatabase,
  file_id: FileId,
  hir_ast: &'a Block,

  locals: HashMap<String, Type>,
  result: Inference,
}

impl<'a> Infer<'a> {
  pub fn new(db: &'a dyn HirDatabase, file_id: FileId, hir_ast: &'a Block) -> Self {
    Infer {
      db,
      file_id,
      hir_ast,
      locals: HashMap::new(),
      result: Inference { types: HashMap::new() },
    }
  }

  pub fn type_expr(&mut self, expr: ExprId) -> Option<Type> {
    let res = match self.hir_ast.exprs[expr] {
      // FIXME: Need name resolution.
      Expr::Name(ref path) => {
        if let Some(local) = self.locals.get(&path.segments[0]) {
          Some(local.clone())
        } else {
          Some(Type {
            path: Path { elems: path.segments.iter().map(|s| Name::new(s.clone())).collect() },
          })
        }
      }

      Expr::Literal(ref lit) => match lit {
        Literal::Int(_) => Some(Type::int()),
        Literal::Float(_) => Some(Type::float()),
        _ => None,
      },

      Expr::Block(block) => self.db.type_of_block(self.file_id, block),

      Expr::FieldAccess(lhs, ref name) => {
        let lhs = self.type_expr(lhs)?;

        // FIXME: Need global lookup here. This should basically be the same as
        // goto-def.
        let scopes = self.db.scopes_of(self.file_id);

        let scope_map = &scopes.scopes[Idx::from_raw(RawIdx::from_u32(0))];
        let (_, def) = scope_map
          .declarations
          .iter()
          .find(|(name, _)| name.as_str() == lhs.path.elems[0].as_str())?;

        // This is basically just "select field `name` off of `def`".
        match def.kind {
          DefinitionKind::Class(Some(body_id)) => {
            let scope = BlockId::Class(AstId::new(def.ast_id));

            let hir_ast = self.db.hir_ast_for_scope(self.file_id, scope);

            let scope_id = scopes.ast_to_scope[&body_id.erased()];
            let scope_def = &scopes.scopes[scope_id];

            let decls: Vec<_> =
              scope_def.declarations.iter().filter(|(n, _)| n.as_str() == name).collect();

            match decls[..] {
              [] => None,
              [(_, def)] => {
                let stmt_id = hir_ast.stmt_map[&def.ast_id];

                match &hir_ast.stmts[stmt_id] {
                  Stmt::Binding(b) => self.db.type_of_expr(self.file_id, scope, b.expr),
                  _ => None,
                }
              }
              _ => None,
            }
          }

          _ => None,
        }
      }

      _ => None,
    };

    if let Some(ty) = res.clone() {
      self.result.types.insert(expr, ty.clone());
    }

    res
  }
}

pub fn infer(db: &dyn HirDatabase, file_id: FileId, scope: BlockId) -> Arc<Inference> {
  let hir_ast = db.hir_ast_for_scope(file_id, scope);

  let mut infer = Infer::new(db, file_id, &hir_ast);

  for &stmt in hir_ast.items.iter() {
    match &hir_ast.stmts[stmt] {
      Stmt::Binding(b) => {
        let ty = infer.type_expr(b.expr);

        if let Some(ty) = ty {
          infer.locals.insert(b.name.clone(), ty);
        }
      }
      Stmt::Expr(e) => {
        infer.type_expr(*e);
      }
    }
  }

  Arc::new(infer.result)
}

pub fn type_of_expr(
  db: &dyn HirDatabase,
  file_id: FileId,
  scope: BlockId,
  expr: ExprId,
) -> Option<Type> {
  let inference = db.infer(file_id, scope);
  inference.types.get(&expr).cloned()
}

pub fn type_of_block(db: &dyn HirDatabase, file_id: FileId, scope: BlockId) -> Option<Type> {
  let hir_ast = db.hir_ast_for_scope(file_id, scope);

  match hir_ast.stmts[*hir_ast.items.last()?] {
    Stmt::Expr(e) => db.type_of_expr(file_id, scope, e),
    _ => None,
  }
}

pub fn type_at(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Option<Type> {
  let ast = db.parse(file_id);

  // FIXME: Dedupe with `scope`
  let node = ast
    .syntax_node()
    .token_at_offset(pos)
    .max_by_key(|token| match token.kind() {
      T![ident] => 10,
      SyntaxKind::INT_LIT_KW => 9,

      // Whitespace is always lowest priority.
      T![nl] => 0,

      _ => 1,
    })
    .unwrap();

  fn block_for_node(ast_id_map: &AstIdMap, mut node: SyntaxNode) -> Option<BlockId> {
    Some(loop {
      scalarc_syntax::match_ast! {
        match node {
          ast::BlockExpr(it) => break BlockId::Block(ast_id_map.ast_id(&it)),
          ast::ClassDef(it) => break BlockId::Class(ast_id_map.ast_id(&it)),
          ast::SourceFile(it) => break BlockId::Source(ast_id_map.ast_id(&it)),
          _ => node = node.parent()?,
        }
      }
    })
  }

  match node.kind() {
    T![ident] => {
      let ast_id_map = db.ast_id_map(file_id);

      if let Some(expr) = ast::Expr::cast(node.parent()?) {
        let scope = block_for_node(&ast_id_map, node.parent()?)?;

        let (_, source_map) = db.hir_ast_with_source_for_scope(file_id, scope);
        let expr_id = source_map.expr(AstPtr::new(&expr))?;

        db.type_of_expr(file_id, scope, expr_id)
      } else {
        let parent = node.parent()?;
        if let Some(val_def) = ast::ValDef::cast(parent) {
          let parent = val_def.syntax().parent()?;
          let scope = block_for_node(&ast_id_map, parent)?;

          let (hir_ast, source_map) = db.hir_ast_with_source_for_scope(file_id, scope);
          let stmt_id = source_map.stmt(AstPtr::new(&val_def.into()))?;
          let stmt = &hir_ast.stmts[stmt_id];

          match stmt {
            crate::ast::Stmt::Binding(b) => db.type_of_expr(file_id, scope, b.expr),
            _ => None,
          }
        } else {
          None
        }
      }
    }

    SyntaxKind::INT_LIT_KW => Some(Type::int()),
    SyntaxKind::FLOAT_LIT_KW => Some(Type::float()),

    SyntaxKind::OPEN_PAREN | SyntaxKind::CLOSE_PAREN => {
      let parent = node.parent()?;

      if scalarc_syntax::ast::TupleExpr::can_cast(parent.kind()) {
        let tup = scalarc_syntax::ast::TupleExpr::cast(parent)?;
        if tup.exprs().count() == 1 {
          db.type_at(file_id, tup.exprs().next()?.syntax().text_range().end())
        } else {
          // TODO: Tuple type
          None
        }
      } else {
        // Parens are the grandchildren of calls, so grab the second parent for this
        // check.
        let parent = parent.parent()?;

        if scalarc_syntax::ast::CallExpr::can_cast(parent.kind()) {
          let call = scalarc_syntax::ast::CallExpr::cast(parent)?;
          let func = call.expr()?;
          let _def = db.def_at_index(file_id, func.syntax().text_range().start())?;

          None
        } else {
          None
        }
      }
    }

    _ => None,
  }
}

pub fn type_at_item(db: &dyn HirDatabase, file_id: FileId, item: ErasedAstId) -> Option<Type> {
  let ast = db.parse(file_id);

  let ast_id_map = db.ast_id_map(file_id);

  let ptr = ast_id_map.get_erased(item);

  let item = ast::Item::cast(ptr.to_node(&ast.syntax_node()))?;

  match item {
    scalarc_syntax::ast::Item::ValDef(_) => db.type_at(file_id, item.syntax().text_range().end()),
    _ => None,
  }
}

impl Type {
  pub fn int() -> Self { Type { path: Path { elems: vec!["scala".into(), "Int".into()] } } }
  pub fn float() -> Self { Type { path: Path { elems: vec!["scala".into(), "Float".into()] } } }
}
