use std::sync::Arc;

use la_arena::Idx;
use scalarc_source::FileId;
use scalarc_syntax::ast;

use crate::{source_map::SyntaxId, HirDatabase};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Name(String);

impl From<&str> for Name {
  fn from(s: &str) -> Self { Name(s.to_owned()) }
}

pub type ItemId = Idx<Item>;
pub type ExprId = Idx<Expr>;

#[derive(Debug, Eq, PartialEq)]
pub struct Package {
  pub items: Box<[Item]>,

  pub arenas: PackageArenas,
}

#[derive(Default, Debug, Eq, PartialEq)]
pub struct PackageArenas {
  def: la_arena::Arena<Def>,
  val: la_arena::Arena<Val>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Def {
  pub name: Name,
  pub args: Box<[Name]>,
  pub body: ExprId,

  pub id: SyntaxId<ast::Def>,
}

impl crate::ItemTreeNode for Def {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Val {
  pub name: Name,
  pub expr: ExprId,

  pub id: SyntaxId<ast::ValDef>,
}

impl crate::ItemTreeNode for Val {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Item {
  Def(Def),
  Val(Val),

  Expr(ExprId),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
  If { cond: ExprId, then_branch: ExprId, else_branch: Option<ExprId> },
  While { cond: ExprId, body: ExprId },
  Try { cond: ExprId, catch: Option<ExprId>, finally: Option<ExprId> },
  DoWhile { body: ExprId, cond: ExprId },
  Throw { expr: ExprId },
  Return { expr: Option<ExprId> },

  Block { block: Block },

  PrefixOp { expr: ExprId, op: Name },
  PostfixOp { expr: ExprId, op: Name },
  InfixOp { lhs: ExprId, rhs: ExprId, op: Name },
  Field { expr: ExprId, field: Name },

  Literal(Literal),
  Tuple { items: Box<[ExprId]> },

  New { name: Name, args: Option<Box<[ExprId]>>, block: Option<Block> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
  Int(i64),
  Float(FloatWrapper),
  Bool(bool),
  String(String),
  Char(char),
  Null,
}

// We don't need ordering, so we just cast f64 bits to a u64 and use `Eq` on
// that. NaN shouldn't ever come up in a literal, so we don't need to worry
// about that.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FloatWrapper(u64);

impl FloatWrapper {
  pub fn new(f: f64) -> Self { FloatWrapper(f.to_bits()) }
  pub fn as_f64(&self) -> f64 { f64::from_bits(self.0) }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block {
  pub items: Box<[ItemId]>,
}

pub fn file_package(db: &dyn HirDatabase, file_id: FileId) -> Arc<Package> {
  let ast = db.parse(file_id);

  let package = crate::lower::lower(db, file_id, ast.tree());

  Arc::new(package)
}
