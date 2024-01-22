// See https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html

use std::ops::Range;

#[derive(Debug)]
pub struct Span {
  pub start: usize,
  pub end:   usize,
}

impl From<Range<usize>> for Span {
  fn from(range: Range<usize>) -> Self { Span { start: range.start, end: range.end } }
}

pub struct Ident {
  pub span: Span,
  pub text: String,
}

// Blocks

pub struct Block {
  pub statements: Vec<Stmt>,
  pub ret:        Option<Expr>,
}

pub enum Stmt {
  Import(Import),
  Definition(Definition),
  Expr(Expr),
}

pub struct Import {
  pub kw: Span,

  // TODO: Fill this in
  pub ident: Ident,
}

pub struct Definition {
  pub annotations: Vec<Annotation>,
  pub implicit:    Option<Span>,
  pub lazy:        Option<Span>,
  pub value:       Def,
}

pub enum Def {
  Val(ValDef),
  Var(VarDef),
  // TODO: Type definitions
}

pub struct ValDef {
  pub kw:    Span,
  pub pat:   Pattern,
  pub ty:    Option<(Span, Type)>,
  pub eq:    Span,
  pub value: Box<Expr>,
}

pub struct VarDef {
  pub kw:   Span,
  pub kind: VarDefKind,
}

pub enum VarDefKind {
  Value { pat: Pattern, ty: Option<(Span, Type)>, eq: Span, value: Box<Expr> },
  Null { id: Ident, colon: Span, ty: Type, eq: Span, underscore: Span },
}

// Patterns

pub struct Pattern {
  pub first: BoundPattern,
  pub rest:  Vec<(Span, BoundPattern)>,
}

pub enum IdentOrUnderscore {
  Ident(Ident),
  Underscore(Span),
}

pub enum BoundPattern {
  Bound { ident: IdentOrUnderscore, colon: Span, ty: Option<Type> },
  Single(SinglePattern),
}

pub enum SinglePattern {
  Named { name: Ident, pat: SimplePattern },
  Simple(SimplePattern),
}

pub enum SimplePattern {
  Underscore(Span),
  Ident(Ident),
  Literal(Literal),
  // TODO: Paren patterns
}

pub struct Literal {
  pub span:  Span,
  pub value: LiteralValue,
}

pub enum LiteralValue {
  Boolean(bool),
  Char(char),
  String(String),
  Int(i64),
  Float(f64),
  Null,
}

pub struct Annotation {
  pub kw:    Span,
  pub ident: Ident,
  // TODO: Annotation arguments
}

// Types

pub enum Type {
  Function(FunctionType),
  Annotated { ty: SimpleType, with: Vec<(Span, SimpleType)> },
}

pub struct FunctionType {
  pub args:  Vec<Box<Type>>,
  pub arrow: Span,
  pub ret:   Box<Type>,
}

pub enum SimpleType {
  Ident(Ident, Option<TypeArgs>),
  Tuple(TupleType),
}

pub struct TypeArgs {
  pub open:  Span,
  pub args:  Vec<Type>,
  pub close: Span,
}

pub struct TupleType {
  pub open:  Span,
  pub types: Vec<Type>,
  pub close: Span,
}

// Expressions

pub struct Expr {
  pub span:  Span,
  pub value: ExprInner,
}

pub enum ExprInner {
  Simple(SimpleExpr),
  Prefix(PrefixExpr),
  Infix(InfixExpr),

  Assign(Assign),
  Match(Match),

  If(If),
  While(While),
  Try(Try),
  Do(Do),
  Throw(Throw),
  Return(Return),
}

pub struct PrefixExpr {
  pub op:   PrefixOp,
  pub expr: SimpleExpr,
}

pub enum PrefixOp {
  Plus,
  Minus,
  Bang,
  Tilde,
}

pub struct InfixExpr {
  pub lhs: Box<Expr>,
  pub op:  Ident,
  pub rhs: Box<Expr>,
}

pub enum SimpleExpr {
  Literal(Literal),
  Ident(Ident),
  Underscore(Span),
  Tuple(TupleExpr),
}

pub struct TupleExpr {
  pub open:  Span,
  pub exprs: Vec<Expr>,
  pub close: Span,
}

pub struct Assign {
  pub lhs: Box<Expr>,
  pub kw:  Span,
  pub rhs: Box<Expr>,
}

pub struct Match {
  pub lhs:   Box<Expr>,
  pub kw:    Span,
  pub cases: Vec<Case>,
}

pub struct Case {
  pub kw:    Span,
  pub pat:   Pattern,
  pub arrow: Span,
  pub block: Block,
}

pub struct If {
  pub kw:    Span,
  pub open:  Span,
  pub cond:  Box<Expr>,
  pub close: Span,
  pub body:  Box<Expr>,
  pub els:   Option<(Span, Box<Expr>)>,
}

pub struct While {
  pub kw:    Span,
  pub open:  Span,
  pub cond:  Box<Expr>,
  pub close: Span,
  pub body:  Box<Expr>,
}

pub struct Try {
  pub kw:      Span,
  pub cond:    Box<Expr>,
  pub catch:   Option<(Span, Box<Expr>)>,
  pub finally: Option<(Span, Box<Expr>)>,
}

pub struct Do {
  pub kw_do:    Span,
  pub body:     Box<Expr>,
  pub kw_while: Span,
  pub open:     Span,
  pub cond:     Box<Expr>,
  pub close:    Span,
}

pub struct Throw {
  pub kw:   Span,
  pub expr: Box<Expr>,
}

pub struct Return {
  pub kw:   Span,
  pub expr: Option<Box<Expr>>,
}
