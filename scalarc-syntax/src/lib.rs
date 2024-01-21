pub mod ast;
pub mod node;

use ast::AstNode;
pub use ast::SourceFile;
pub use error::SyntaxError;

mod error;
mod parse;

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
    let (green, mut errors) = parse::parse_text(text);
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
  let source_code = "def foo = 1 + 1\n";
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
  dbg!(&file);
  for item in file.top_stat_seq() {
    dbg!(item);
  }
  let func: ast::FunDec = func.unwrap();

  /*
  // Each AST node has a bunch of getters for children. All getters return
  // `Option`s though, to account for incomplete code. Some getters are common
  // for several kinds of node. In this case, a trait like `ast::NameOwner`
  // usually exists. By convention, all ast types should be used with `ast::`
  // qualifier.
  let name: Option<ast::Path> = func.name();
  let name = name.unwrap();
  assert_eq!(name.text(), "foo");

  // Let's get the `1 + 1` expression!
  let body: ast::BlockExpr = func.body().unwrap();
  let stmt_list: ast::StmtList = body.stmt_list().unwrap();
  let expr: ast::Expr = stmt_list.tail_expr().unwrap();

  // Enums are used to group related ast nodes together, and can be used for
  // matching. However, because there are no public fields, it's possible to
  // match only the top level enum: that is the price we pay for increased API
  // flexibility
  let bin_expr: &ast::BinExpr = match &expr {
    ast::Expr::BinExpr(e) => e,
    _ => unreachable!(),
  };
  */
}
