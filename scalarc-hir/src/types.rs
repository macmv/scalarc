//! Type inferrence.
//!
//! This should probably be its own crate. Ah well.

use std::{collections::HashMap, fmt, sync::Arc};

use crate::{
  hir::{
    self, AstId, BindingKind, Block, BlockId, ErasedAstId, Expr, ExprId, Literal, Pattern,
    PatternId, ResolutionKind, Stmt, StmtId,
  },
  DefinitionKey, GlobalDefinition, GlobalDefinitionKind, HirDatabase, HirDefinition,
  HirDefinitionId, HirDefinitionKind, InFile, InFileExt, InferQuery, Name, Path,
};
use salsa::{Query, QueryDb};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode, SyntaxKind},
  AstPtr, SyntaxNodePtr, TextSize, T,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
  Unknown,
  Object(Path),
  Instance(Path),
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
  pub fn empty() -> Self { Signature { params: vec![], ret: None } }

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
                  Type::Instance(Path {
                    elems: vec![Name("scala".into()), Name(ty.syntax().text().into())],
                  }),
                ))
              } else {
                None
              }
            })
            .collect(),
        })
        .collect(),
      ret:    sig.ty().map(|ty| {
        Type::Instance(Path { elems: vec![Name("scala".into()), Name(ty.syntax().text().into())] })
      }),
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
      Type::Unknown => write!(f, "unknown"),
      Type::Instance(path) => write!(f, "{}", path),
      Type::Object(path) => write!(f, "${}", path),
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
  exprs:    HashMap<ExprId, Type>,
  stmts:    HashMap<StmtId, Type>,
  patterns: HashMap<PatternId, Type>,
}

struct Infer<'a> {
  db:       &'a dyn HirDatabase,
  block_id: InFile<BlockId>,
  hir_ast:  &'a Block,

  locals: HashMap<String, Type>,
  result: Inference,
}

impl<'a> Infer<'a> {
  pub fn new(db: &'a dyn HirDatabase, block_id: InFile<BlockId>, hir_ast: &'a Block) -> Self {
    let mut infer = Infer {
      db,
      hir_ast,
      block_id,
      locals: HashMap::new(),
      result: Inference {
        exprs:    HashMap::new(),
        stmts:    HashMap::new(),
        patterns: HashMap::new(),
      },
    };

    for param in infer.hir_ast.params.values() {
      if let Some(ref ty) = param.ty {
        if let Some(ty) = infer.type_te(ty) {
          infer.locals.insert(param.name.clone(), ty);
        }
      }
    }

    infer
  }

  pub fn type_expr(&mut self, expr: ExprId) -> Option<Type> {
    let res = match self.hir_ast.exprs[expr] {
      Expr::Name(ref path) => {
        if let Some(local) = self.locals.get(&path.segments[0]) {
          Some(local.clone())
        } else {
          let name = &path.segments[0];
          match self.db.lookup_name_in_block(self.block_id, name.clone()) {
            Some(def) => self.type_of_hir_def(def),
            None => {
              let path = Path { elems: vec![name.clone().into()] };

              let target = self.db.file_target(self.block_id.file_id)?;
              let def = self.db.definition_for_key(target, DefinitionKey::Object(path))?;

              // NB: The name `Int` on its own refers to an object, not a class.
              Some(Type::Object(def.path))
            }
          }
        }
      }

      Expr::New(ref path, ref args) => {
        // TODO: Do we care about the arguments?
        for arg in args {
          let _ = self.type_expr(*arg);
        }

        let path =
          self.db.resolve_path_in_block(self.block_id, path.clone(), ResolutionKind::Instance)?;

        let target = self.db.file_target(self.block_id.file_id)?;
        let def = self.db.definition_for_key(target, DefinitionKey::Instance(path))?;

        // NB: The name `new Int()` on its own refers to a class, not an object.
        Some(Type::Instance(def.path))
      }

      Expr::Literal(ref lit) => match lit {
        Literal::Int(_) => Some(Type::int()),
        Literal::Float(_) => Some(Type::float()),
        _ => None,
      },

      Expr::Block(block) => self.db.type_of_block(block.in_file(self.block_id.file_id)),

      Expr::FieldAccess(lhs, ref name) => self.type_access(lhs, name),
      Expr::Call(lhs, ref args) => {
        if let Type::Lambda(params, ret) = self.type_expr(lhs)? {
          if params.len() != args.len() {
            return None;
          }

          for (param, arg) in params.iter().zip(args.iter()) {
            let arg_ty = self.type_expr(*arg)?;

            // TODO: Subtyping!
            if arg_ty != *param {
              return None;
            }
          }

          Some(*ret)
        } else {
          None
        }
      }

      Expr::Tuple(ref items) => {
        let mut types = Vec::with_capacity(items.len());

        for &item in items {
          types.push(self.type_expr(item)?);
        }

        Some(Type::Tuple(types))
      }

      Expr::Match(lhs, ref arms) => {
        let lhs = self.type_expr(lhs)?;

        let mut ty = None;

        for &(pat, arm) in arms {
          self.type_pattern(&lhs, pat);
          let arm_ty = self.type_expr(arm)?;

          match ty {
            None => ty = Some(arm_ty),
            Some(ref t) if *t == arm_ty => {}
            Some(_) => {
              // TODO: Supertype of `t` and `arm_ty`!
              //
              // For now we just use `t`.
            }
          }
        }

        ty
      }

      _ => None,
    };

    if let Some(ty) = res.clone() {
      self.result.exprs.insert(expr, ty.clone());
    }

    res
  }

  /// Fully resolves an HIR type into a concrete type.
  fn type_te(&self, te: &hir::Type) -> Option<Type> {
    match te {
      hir::Type::Unknown => Some(Type::Unknown),
      hir::Type::Named(ref path) => {
        let path =
          self.db.resolve_path_in_block(self.block_id, path.clone(), ResolutionKind::Instance)?;

        let target = self.db.file_target(self.block_id.file_id)?;
        let def = self.db.definition_for_key(target, DefinitionKey::Instance(path))?;

        // NB: The type `Int` in a type expression refers to the `Int` class, not the
        // `Int` object.
        Some(Type::Instance(def.path))
      }
    }
  }

  fn type_of_hir_def(&self, def: HirDefinition) -> Option<Type> {
    match def.kind {
      HirDefinitionKind::Val(Some(ty)) => self.type_te(&ty),
      HirDefinitionKind::Var(Some(ty)) => self.type_te(&ty),
      HirDefinitionKind::Parameter(ty) => self.type_te(&ty),
      HirDefinitionKind::Import => {
        let path = self.db.resolve_path_in_block(
          self.block_id,
          hir::UnresolvedPath::from_name(def.name.clone().into_string()),
          ResolutionKind::Object,
        )?;

        let target = self.db.file_target(self.block_id.file_id)?;
        let def = self.db.definition_for_key(target, DefinitionKey::Object(path))?;

        Some(Type::Object(def.path))
      }
      _ => {
        // This is tricky. We need to infer the type of the parent block, and we need to
        // avoid recursion.

        match def.id {
          HirDefinitionId::Stmt(block) => {
            let inferred = try_infer(self.db, def.block_id, Some(block))?;
            inferred.stmts.get(&block).cloned()
          }
          _ => None,
        }
      }
    }
  }

  fn type_access(&mut self, lhs: ExprId, name: &str) -> Option<Type> {
    let lhs = self.type_expr(lhs)?;
    let key = match lhs {
      Type::Object(ref path) => DefinitionKey::Object(path.clone()),
      Type::Instance(ref path) => DefinitionKey::Instance(path.clone()),
      _ => return None,
    };

    for target in self.db.workspace().all_dependencies(self.db.file_target(self.block_id.file_id)?)
    {
      let defs = self.db.definitions_for_target(target);

      if let Some(def) = defs.items.get(&key) {
        return self.select_name_from_def(def, name);
      }
    }

    None
  }

  fn type_pattern(&mut self, lhs: &Type, pat: PatternId) {
    match self.hir_ast.patterns[pat] {
      Pattern::Wildcard => {}
      Pattern::Binding(_) => {
        self.result.patterns.insert(pat, lhs.clone());
      }
    }
  }

  fn select_name_from_def(&self, def: &GlobalDefinition, name: &str) -> Option<Type> {
    let block = match def.kind {
      GlobalDefinitionKind::Class(Some(_), _) => {
        BlockId::Class(AstId::new(def.ast_id)).in_file(def.file_id)
      }
      GlobalDefinitionKind::Trait(Some(_)) => {
        BlockId::Trait(AstId::new(def.ast_id)).in_file(def.file_id)
      }
      GlobalDefinitionKind::Object(Some(_)) => {
        BlockId::Object(AstId::new(def.ast_id)).in_file(def.file_id)
      }
      _ => return None,
    };

    let hir_ast = self.db.hir_ast_for_block(block);
    let inferred = try_infer(self.db, block, None)?;

    // Find all the `def` and `val`s in the block.
    let decls: Vec<_> = hir_ast
      .items
      .iter()
      .filter_map(|&it| match hir_ast.stmts[it] {
        hir::Stmt::Binding(ref b) => match b.kind {
          BindingKind::Def(_) | BindingKind::Val => {
            if b.name == name {
              Some(it)
            } else {
              None
            }
          }
          _ => None,
        },
        _ => None,
      })
      .collect();

    match decls[..] {
      [] => None,
      [stmt_id] => inferred.stmts.get(&stmt_id).cloned(),
      _ => {
        // TODO: Resolve overloads.
        let stmt_id = decls.first().unwrap();

        inferred.stmts.get(&stmt_id).cloned()
      }
    }
  }
}

pub fn infer(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  stop_at: Option<StmtId>,
) -> Arc<Inference> {
  let hir_ast = db.hir_ast_for_block(block);

  let mut infer = Infer::new(db, block, &hir_ast);

  for &stmt in hir_ast.items.iter() {
    if let Some(stop) = stop_at {
      if stmt > stop {
        break;
      }
    }

    match &hir_ast.stmts[stmt] {
      Stmt::Binding(b) => match b.kind {
        BindingKind::Def(ref sig) => {
          let inferred = match b.expr {
            // Only infer the block if the function signature is incomplete.
            Some(expr) if sig.ret.is_none() => infer.type_expr(expr).unwrap_or(Type::Unknown),
            _ => Type::Unknown,
          };

          // Functions without any parameters don't require parameters to be called, so
          // this little loop just works.
          let mut result = sig.ret.clone().unwrap_or(inferred);
          for params in sig.params.iter().rev() {
            result = Type::Lambda(
              params.params.iter().map(|(_, ty)| ty.clone()).collect(),
              Box::new(result),
            );
          }

          infer.result.stmts.insert(stmt, result.clone());
          infer.locals.insert(b.name.clone(), result);
        }
        _ => {
          if let Some(expr) = b.expr {
            if let Some(body_ty) = infer.type_expr(expr) {
              let ty = match b.kind {
                BindingKind::Val => body_ty,
                BindingKind::Var => body_ty,
                BindingKind::Def(_) => unreachable!(),
                BindingKind::Pattern => todo!(),
              };

              infer.result.stmts.insert(stmt, ty.clone());
              infer.locals.insert(b.name.clone(), ty);
            } else {
              infer.result.stmts.insert(stmt, Type::Unknown);
              infer.locals.insert(b.name.clone(), Type::Unknown);
            }
          } else {
            infer.result.stmts.insert(stmt, Type::Unknown);
            infer.locals.insert(b.name.clone(), Type::Unknown);
          }
        }
      },
      Stmt::Expr(e) => {
        infer.type_expr(*e);
      }
    }
  }

  // Special case defs, as they have a single expression, with no items.
  if hir_ast.items.is_empty() {
    for (expr, _) in hir_ast.exprs.iter() {
      infer.type_expr(expr);
    }
  }

  Arc::new(infer.result)
}

// TODO: This hack needs more validation. In short, if you have a recursive
// function call with no explicit signatures, then `infer` will create a cyclic
// depdendency. Because this is invalid scala code in the first place, and its a
// rare occurance, we just hack around that case, to make the common path more
// cache efficient.
fn get_query_storage<'me, Q>(db: &'me <Q as QueryDb<'me>>::DynDb) -> &'me Q::Storage
where
  Q: Query + 'me,
  Q::Storage: salsa::plumbing::QueryStorageOps<Q>,
{
  let group_storage: &Q::GroupStorage = salsa::plumbing::HasQueryGroup::group_storage(db);
  let query_storage: &Q::Storage = Q::query_storage(group_storage);
  query_storage
}

fn try_infer(
  db: &dyn HirDatabase,
  block: InFile<BlockId>,
  stop_at: Option<StmtId>,
) -> Option<Arc<Inference>> {
  let storage = get_query_storage::<InferQuery>(db);

  use salsa::plumbing::QueryStorageOps;
  match storage.try_fetch(db, &(block, stop_at)) {
    Ok(v) => Some(v),
    Err(_) => None,
  }
}

pub fn type_of_expr(db: &dyn HirDatabase, block: InFile<BlockId>, expr: ExprId) -> Option<Type> {
  let inferred = try_infer(db, block, None)?;
  inferred.exprs.get(&expr).cloned()
}

pub fn type_of_block(db: &dyn HirDatabase, block: InFile<BlockId>) -> Option<Type> {
  let hir_ast = db.hir_ast_for_block(block);

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

        let source_map = db.hir_source_map_for_block(block);
        let expr_id = source_map.expr(AstPtr::new(&expr))?;

        db.type_of_expr(block, expr_id)
      } else {
        // FIXME: This needs to search upwards until it finds something interesting.
        // Right now its stuck searching a fixed amount, which doesn't work
        // well.
        let parent2 = node.parent()?.parent()?;
        if let Some(pat) = ast::Pattern::cast(parent2) {
          let parent = pat.syntax().parent()?;
          if let Some(val_def) = ast::ValDef::cast(parent) {
            let parent = val_def.syntax().parent()?;
            let block = db.block_for_node(SyntaxNodePtr::new(&parent).in_file(file_id));

            let (hir_ast, source_map) = db.hir_ast_with_source_for_block(block);
            let stmt_id = source_map.stmt(AstPtr::new(&val_def.into()))?;
            let stmt = &hir_ast.stmts[stmt_id];

            match stmt {
              crate::hir::Stmt::Binding(b) => match b.expr {
                Some(expr) => db.type_of_expr(block, expr),
                None => None,
              },
              _ => None,
            }
          } else {
            None
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
      let (_, source_map) = db.hir_ast_with_source_for_block(block);

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
  pub fn int() -> Self { Type::Instance(Path { elems: vec!["scala".into(), "Int".into()] }) }
  pub fn float() -> Self { Type::Instance(Path { elems: vec!["scala".into(), "Float".into()] }) }
}
