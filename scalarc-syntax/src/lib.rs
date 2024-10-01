pub mod ast;
pub mod node;

use ast::AstNode;
pub use ast::SourceFile;
pub use error::SyntaxError;
pub use scalarc_parser::T;

mod error;
mod parse;

#[cfg(test)]
mod tests;

use std::{
  hash::{Hash, Hasher},
  marker::PhantomData,
  sync::Arc,
};

use node::{Scala, SyntaxNode};
use rowan::GreenNode;
use scalarc_parser::SyntaxKind;

/// `Parse` is the result of the parsing: a syntax tree and a collection of
/// errors.
///
/// Note that we always produce a syntax tree, even for completely invalid
/// files.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Parse<T> {
  green:  GreenNode,
  errors: Option<Arc<[SyntaxError]>>,
  _ty:    PhantomData<fn() -> T>,
}

pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<Scala>;

#[derive(Debug, PartialEq, Eq)]
pub struct AstPtr<T> {
  ptr:      SyntaxNodePtr,
  _phantom: std::marker::PhantomData<fn() -> T>,
}

impl<T> Clone for AstPtr<T> {
  fn clone(&self) -> Self { AstPtr { ptr: self.ptr, _phantom: PhantomData } }
}
impl<T> Copy for AstPtr<T> {}

impl<T> Hash for AstPtr<T> {
  fn hash<H: Hasher>(&self, state: &mut H) { self.ptr.hash(state) }
}

impl<T: AstNode> AstPtr<T> {
  pub fn new(node: &T) -> Self {
    AstPtr { ptr: SyntaxNodePtr::new(node.syntax()), _phantom: PhantomData }
  }

  pub fn to_node(&self, root: &Parse<SourceFile>) -> SyntaxNode {
    self.ptr.to_node(&root.syntax_node())
  }
}

#[macro_export]
macro_rules! match_ast {
  (match $node:ident { $($tt:tt)* }) => { $crate::match_ast!(match ($node) { $($tt)* }) };

  (match ($node:expr) {
    $( $( $path:ident )::+ ($it:pat) => $res:expr, )*
    _ => $catch_all:expr $(,)?
  }) => {{
    $( if let Some($it) = $($path::)+cast($node.clone()) { $res } else )*
    { $catch_all }
  }};
}

pub use rowan::{TextRange, TextSize, WalkEvent};

impl<T> Parse<T> {
  pub fn syntax_node(&self) -> SyntaxNode { SyntaxNode::new_root(self.green.clone()) }
  pub fn errors(&self) -> &[SyntaxError] { self.errors.as_deref().unwrap_or_default() }
}

impl<T: AstNode> Parse<T> {
  pub fn tree(&self) -> T { T::cast(self.syntax_node()).unwrap() }
}

impl SourceFile {
  pub fn parse(text: &str) -> Parse<SourceFile> {
    let (green, errors) = parse::parse_text(text);
    let root = SyntaxNode::new_root(green.clone());

    // TODO: Add validation :P. Much easier said than done.
    // errors.extend(validation::validate(&root));

    assert_eq!(root.kind(), SyntaxKind::SOURCE_FILE);
    Parse {
      green,
      errors: if errors.is_empty() { None } else { Some(errors.into()) },
      _ty: PhantomData,
    }
  }
}

/// This test does not assert anything and instead just shows off the crate's
/// API.
#[test]
fn api_walkthrough() {
  let source_code = "def foo = x + 2\n";
  // `SourceFile` is the main entry point.
  //
  // The `parse` method returns a `Parse` -- a pair of syntax tree and a list
  // of errors. That is, syntax tree is constructed even in presence of errors.
  let parse = SourceFile::parse(source_code);
  assert!(parse.errors().is_empty());

  // The `tree` method returns an owned syntax node of type `SourceFile`.
  // Owned nodes are cheap: inside, they are `Rc` handles to the underling data.
  let file: SourceFile = parse.tree();

  // `SourceFile` is the root of the syntax tree. We can iterate file's items.
  // Let's fetch the `foo` function.
  let mut func = None;
  for item in file.items() {
    // The `fun_sig` is the name, arguments, and return type. In this case, it will
    // just be the name `foo`.
    let ast::Item::FunDef(item) = item else { panic!() };
    if item.fun_sig().unwrap().id_token().unwrap().text() == "foo" {
      func = Some(item);
    }
  }
  let func: ast::FunDef = func.unwrap();

  // The `expr` of a function is everything on the right of the `=`. We can match
  // on this expression and grab the inner literals of `1 + 2`.
  let ast::Expr::InfixExpr(infix) = func.expr().unwrap() else { panic!() };

  let ast::Expr::IdentExpr(id) = infix.lhs().unwrap() else { panic!() };
  assert_eq!(id.id_token().unwrap().text(), "x");

  let ast::Expr::LitExpr(rhs) = infix.rhs().unwrap() else { panic!() };
  assert_eq!(rhs.int_lit_token().unwrap().text(), "2");
}
