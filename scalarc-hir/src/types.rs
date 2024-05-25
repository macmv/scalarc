//! Type inferrence.
//!
//! This should probably be its own crate. Ah well.

use std::{collections::HashMap, fmt, sync::Arc};

use crate::{
  hir::{AstId, BindingKind, Block, BlockId, ErasedAstId, Expr, ExprId, Literal, Stmt},
  DefinitionKind, HirDatabase, InFile, InFileExt, Name, Path,
};
use la_arena::{Idx, RawIdx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode, SyntaxKind},
  AstPtr, SyntaxNodePtr, TextSize, T,
};

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
  Named(Path),
  Tuple(Vec<Type>),
  Lambda(Vec<Type>, Box<Type>),
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

impl Signature {
  pub fn from_ast(sig: &ast::FunSig) -> Self {
    Signature {
      params: sig
        .fun_paramss()
        .map(|p| Params {
          implicit: false,
          params:   p
            .fun_params()
            .filter_map(|p| {
              if let (Some(id), Some(ty)) = (p.id_token(), p.ty()) {
                Some((
                  id.text().into(),
                  Type::Named(Path { elems: vec![Name(ty.syntax().text().into())] }),
                ))
              } else {
                None
              }
            })
            .collect(),
        })
        .collect(),
      ret:    None,
    }
  }
}

impl fmt::Debug for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "Type({})", self) }
}
impl fmt::Debug for Signature {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "Signature({})", self) }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::Named(path) => write!(f, "{}", path),
      Type::Tuple(types) => {
        write!(f, "(")?;
        for (i, ty) in types.iter().enumerate() {
          if i != 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", ty)?;
        }
        write!(f, ")")
      }
      Type::Lambda(args, ret) => {
        write!(f, "(")?;
        for (i, ty) in args.iter().enumerate() {
          if i != 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", ty)?;
        }
        write!(f, ") => ")?;
        write!(f, "{}", ret)
      }
    }
  }
}

impl fmt::Display for Path {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, elem) in self.elems.iter().enumerate() {
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
          Some(Type::Named(Path {
            elems: path.segments.iter().map(|s| Name::new(s.clone())).collect(),
          }))
        }
      }

      Expr::Literal(ref lit) => match lit {
        Literal::Int(_) => Some(Type::int()),
        Literal::Float(_) => Some(Type::float()),
        _ => None,
      },

      Expr::Block(block) => self.db.type_of_block(block.in_file(self.file_id)),

      Expr::FieldAccess(lhs, ref name) => {
        let lhs = self.type_expr(lhs)?;

        // FIXME: Need global lookup here. This should basically be the same as
        // goto-def.
        let scopes = self.db.scopes_of(self.file_id);

        let scope_map = &scopes.scopes[Idx::from_raw(RawIdx::from_u32(0))];
        let (_, def) = scope_map.declarations.iter().find(
          |(name, _)| matches!(lhs, Type::Named(ref path) if path.elems[0].as_str() == name.as_str()),
        )?;

        // This is basically just "select field `name` off of `def`".
        match def.kind {
          DefinitionKind::Class(Some(body_id)) => {
            // FIXME: This should use a different file_id.
            let block = BlockId::Class(AstId::new(def.ast_id)).in_file(self.file_id);

            let hir_ast = self.db.hir_ast_for_scope(block);

            let scope_id = scopes.ast_to_scope[&body_id.erased()];
            let scope_def = &scopes.scopes[scope_id];

            let decls: Vec<_> =
              scope_def.declarations.iter().filter(|(n, _)| n.as_str() == name).collect();

            match decls[..] {
              [] => None,
              [(_, def)] => {
                let stmt_id = hir_ast.stmt_map[&def.ast_id];

                match &hir_ast.stmts[stmt_id] {
                  Stmt::Binding(b) => {
                    let body_ty = self.db.type_of_expr(block, b.expr)?;

                    Some(match b.kind {
                      BindingKind::Val => body_ty,
                      BindingKind::Var => body_ty,
                      BindingKind::Def(ref sig) => {
                        let mut ty = body_ty;

                        for params in sig.params.iter().rev() {
                          ty = Type::Lambda(
                            params.params.iter().map(|(_, ty)| ty.clone()).collect(),
                            Box::new(ty),
                          );
                        }

                        ty
                      }
                    })
                  }
                  _ => None,
                }
              }
              _ => None,
            }
          }

          _ => None,
        }
      }

      Expr::Tuple(ref items) => {
        let mut types = Vec::with_capacity(items.len());

        for &item in items {
          types.push(self.type_expr(item)?);
        }

        Some(Type::Tuple(types))
      }

      _ => None,
    };

    if let Some(ty) = res.clone() {
      self.result.types.insert(expr, ty.clone());
    }

    res
  }
}

pub fn infer(db: &dyn HirDatabase, block: InFile<BlockId>) -> Arc<Inference> {
  let hir_ast = db.hir_ast_for_scope(block);

  let mut infer = Infer::new(db, block.file_id, &hir_ast);

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

pub fn type_of_expr(db: &dyn HirDatabase, block: InFile<BlockId>, expr: ExprId) -> Option<Type> {
  let inference = db.infer(block);
  inference.types.get(&expr).cloned()
}

pub fn type_of_block(db: &dyn HirDatabase, block: InFile<BlockId>) -> Option<Type> {
  let hir_ast = db.hir_ast_for_scope(block);

  match hir_ast.stmts[*hir_ast.items.last()?] {
    Stmt::Expr(e) => db.type_of_expr(block, e),
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

  match node.kind() {
    T![ident] => {
      if let Some(expr) = ast::Expr::cast(node.parent()?) {
        let block = db.block_for_node(SyntaxNodePtr::new(&node.parent()?).in_file(file_id));

        let (_, source_map) = db.hir_ast_with_source_for_scope(block);
        let expr_id = source_map.expr(AstPtr::new(&expr))?;

        db.type_of_expr(block, expr_id)
      } else {
        let parent = node.parent()?;
        if let Some(val_def) = ast::ValDef::cast(parent) {
          let parent = val_def.syntax().parent()?;
          let block = db.block_for_node(SyntaxNodePtr::new(&parent).in_file(file_id));

          let (hir_ast, source_map) = db.hir_ast_with_source_for_scope(block);
          let stmt_id = source_map.stmt(AstPtr::new(&val_def.into()))?;
          let stmt = &hir_ast.stmts[stmt_id];

          match stmt {
            crate::hir::Stmt::Binding(b) => db.type_of_expr(block, b.expr),
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
      let block = db.block_for_node(SyntaxNodePtr::new(&parent).in_file(file_id));
      let (_, source_map) = db.hir_ast_with_source_for_scope(block);

      if scalarc_syntax::ast::TupleExpr::can_cast(parent.kind()) {
        let tup = scalarc_syntax::ast::TupleExpr::cast(parent)?;

        let expr_id = source_map.expr(AstPtr::new(&tup.into()))?;
        db.type_of_expr(block, expr_id)
      } else {
        // Parens are the grandchildren of calls, so grab the second parent for this
        // check.
        let parent = parent.parent()?;

        if scalarc_syntax::ast::CallExpr::can_cast(parent.kind()) {
          let call = scalarc_syntax::ast::CallExpr::cast(parent)?;

          let expr_id = source_map.expr(AstPtr::new(&call.into()))?;
          db.type_of_expr(block, expr_id)
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
  pub fn int() -> Self { Type::Named(Path { elems: vec!["scala".into(), "Int".into()] }) }
  pub fn float() -> Self { Type::Named(Path { elems: vec!["scala".into(), "Float".into()] }) }
}
