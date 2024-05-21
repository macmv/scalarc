//! Type inferrence.
//!
//! This should probably be its own crate. Ah well.

use std::fmt;

use crate::{
  ast::{AstId, ErasedAstId, Expr, ExprId, Literal, Stmt, StmtId},
  tree::{ItemId, Name},
  HirDatabase, Path,
};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode, BlockExpr, SyntaxKind},
  TextSize, T,
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

pub fn type_of_expr(
  db: &dyn HirDatabase,
  file_id: FileId,
  scope: Option<AstId<BlockExpr>>,
  expr: ExprId,
) -> Option<Type> {
  let hir_ast = db.hir_ast_for_scope(file_id, scope);

  let expr = &hir_ast.exprs[expr];

  match expr {
    Expr::Literal(lit) => match lit {
      Literal::Int(_) => Some(Type::int()),
      Literal::Float(_) => Some(Type::float()),
      _ => None,
    },

    Expr::Block(block) => db.type_of_block(file_id, Some(*block)),

    _ => None,
  }
}

pub fn type_of_block(
  db: &dyn HirDatabase,
  file_id: FileId,
  scope: Option<AstId<BlockExpr>>,
) -> Option<Type> {
  let hir_ast = db.hir_ast_for_scope(file_id, scope);

  match hir_ast.stmts[*hir_ast.items.last()?] {
    Stmt::Expr(e) => db.type_of_expr(file_id, scope, e),
    _ => None,
  }
}

// FIXME: This should be removed. Its borked.
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

  dbg!(&node.kind());

  match node.kind() {
    T![ident] => {
      let item_id_map = db.item_id_map(file_id);

      let parent = node.parent()?;
      if let Some(val_def) = ast::ValDef::cast(parent) {
        let val_id = item_id_map.item_id(&val_def);

        let parent = val_def.syntax().parent()?;

        let scope = if let Some(block) = ast::BlockExpr::cast(parent) {
          let block_id = item_id_map.item_id(&block);
          Some(block_id)
        } else {
          // TODO: Other parents might exist. For now, we assume the parent is the source
          // root.
          None
        };

        let hir_ast = db.hir_ast_for_scope(file_id, scope);
        let stmt = hir_ast.item_for_ast_id(val_id.erased())?;

        match stmt {
          crate::ast::Stmt::Binding(b) => db.type_of_expr(file_id, scope, b.expr),
          _ => return None,
        }
      } else {
        None
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
          let def = db.def_at_index(file_id, func.syntax().text_range().start())?;

          dbg!(&def);

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

  let item_id_map = db.item_id_map(file_id);

  let ptr = item_id_map.get_erased(item);

  let item = ast::Item::cast(ptr.to_node(&ast.syntax_node()))?;

  dbg!(&item);

  match item {
    scalarc_syntax::ast::Item::ValDef(_) => db.type_at(file_id, item.syntax().text_range().end()),
    _ => None,
  }
}

impl Type {
  pub fn int() -> Self { Type { path: Path { elems: vec!["scala".into(), "Int".into()] } } }
  pub fn float() -> Self { Type { path: Path { elems: vec!["scala".into(), "Float".into()] } } }
}
