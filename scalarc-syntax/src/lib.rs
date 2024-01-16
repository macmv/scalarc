pub mod ast;
pub mod node;

pub use crate::ast::SourceFile;

/// This test does not assert anything and instead just shows off the crate's
/// API.
#[test]
fn api_walkthrough() {
  let source_code = "
    def foo = 1 + 1
  ";
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
