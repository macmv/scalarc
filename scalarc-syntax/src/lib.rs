pub mod ast;
pub mod node;

use ast::AstNode;
pub use ast::SourceFile;
pub use error::SyntaxError;

mod error;
mod parse;

#[cfg(test)]
mod tests;

use std::{marker::PhantomData, sync::Arc};

use node::SyntaxNode;
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
  let source_code = "def foo = 1 + 2\n";
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

  let ast::Expr::LitExpr(lhs) = infix.lhs().unwrap() else { panic!() };
  assert_eq!(lhs.int_lit_token().unwrap().text(), "1");

  let ast::Expr::LitExpr(rhs) = infix.rhs().unwrap() else { panic!() };
  assert_eq!(rhs.int_lit_token().unwrap().text(), "2");
}
