use super::{AstId, ErasedAstId};
use crate::Signature;
use hashbrown::HashMap;
use la_arena::{Arena, Idx};
use scalarc_syntax::ast;

pub type StmtId = Idx<Stmt>;
pub type ExprId = Idx<Expr>;
pub type ParamId = Idx<Binding>;

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
  pub stmts: Arena<Stmt>,
  pub exprs: Arena<Expr>,

  // After parsing to a `Block`, we need a way to lookup an AST item under the cursor, and convert
  // it to a statement. This map is used for that.
  pub stmt_map: HashMap<ErasedAstId, StmtId>,

  // Parameters of this block. This will be set if this block is for a function or class body.
  pub params: Arena<Binding>,

  // Items in the block.
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
  pub expr:     Option<ExprId>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BindingKind {
  Val,
  Var,
  Def(Signature),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
  // Nested blocks go through HirDatabase.
  Block(BlockId),

  Literal(Literal),
  Name(UnresolvedPath),
  Underscore,
  Tuple(Vec<ExprId>),

  FieldAccess(ExprId, String),
  Call(ExprId, Vec<ExprId>),

  New(UnresolvedPath, Vec<ExprId>),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockId {
  Block(AstId<ast::BlockExpr>),
  Def(AstId<ast::FunDef>),
  Class(AstId<ast::ClassDef>),
  Trait(AstId<ast::TraitDef>),
  Object(AstId<ast::ObjectDef>),
  Source(AstId<ast::SourceFile>),
}

impl From<AstId<ast::BlockExpr>> for BlockId {
  fn from(id: AstId<ast::BlockExpr>) -> Self { BlockId::Block(id) }
}
impl From<AstId<ast::FunDef>> for BlockId {
  fn from(id: AstId<ast::FunDef>) -> Self { BlockId::Def(id) }
}
impl From<AstId<ast::ClassDef>> for BlockId {
  fn from(id: AstId<ast::ClassDef>) -> Self { BlockId::Class(id) }
}
impl From<AstId<ast::TraitDef>> for BlockId {
  fn from(id: AstId<ast::TraitDef>) -> Self { BlockId::Trait(id) }
}
impl From<AstId<ast::ObjectDef>> for BlockId {
  fn from(id: AstId<ast::ObjectDef>) -> Self { BlockId::Object(id) }
}
impl From<AstId<ast::SourceFile>> for BlockId {
  fn from(id: AstId<ast::SourceFile>) -> Self { BlockId::Source(id) }
}

impl BlockId {
  pub fn erased(&self) -> ErasedAstId {
    match self {
      BlockId::Block(id) => id.erased(),
      BlockId::Def(id) => id.erased(),
      BlockId::Class(id) => id.erased(),
      BlockId::Trait(id) => id.erased(),
      BlockId::Object(id) => id.erased(),
      BlockId::Source(id) => id.erased(),
    }
  }
}
