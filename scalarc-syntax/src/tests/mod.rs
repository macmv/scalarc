use crate::{ast, SourceFile};

fn parse_ok(src: &str) -> crate::Parse<SourceFile> {
  let parse = SourceFile::parse(src);
  assert!(parse.errors().is_empty());
  parse
}

#[test]
fn classes() {
  let parse = parse_ok("class Foo() {}");
  let file: SourceFile = parse.tree();

  assert_eq!(file.items().count(), 1);
  let ast::Item::ClassDef(class) = file.items().next().unwrap() else { panic!() };
  assert_eq!(class.id_token().unwrap().text(), "Foo");
  assert_eq!(class.body().unwrap().items().count(), 0);

  let parse = parse_ok("class Foo() { class Bar() {} }");
  let file: SourceFile = parse.tree();

  assert_eq!(file.items().count(), 1);
  let ast::Item::ClassDef(class) = file.items().next().unwrap() else { panic!() };
  assert_eq!(class.id_token().unwrap().text(), "Foo");
  assert_eq!(class.body().unwrap().items().count(), 1);
  let ast::Item::ClassDef(class) = class.body().unwrap().items().next().unwrap() else { panic!() };
  assert_eq!(class.id_token().unwrap().text(), "Bar");
  assert_eq!(class.body().unwrap().items().count(), 0);
}

#[test]
fn call_expr() {
  let parse = parse_ok("def foo = println(3)");
  let file: SourceFile = parse.tree();

  assert_eq!(file.items().count(), 1);
  let ast::Item::FunDef(func) = file.items().next().unwrap() else { panic!() };
  assert_eq!(func.fun_sig().unwrap().id_token().unwrap().text(), "foo");
  let ast::Expr::CallExpr(call) = func.expr().unwrap() else { panic!() };

  let ast::Arguments::ParenArguments(args) = call.arguments().unwrap() else { panic!() };
  let args = args.exprs().collect::<Vec<_>>();
  assert_eq!(args.len(), 1);
  let ast::Expr::LitExpr(lit) = &args[0] else { panic!() };

  assert_eq!(lit.int_lit_token().unwrap().text(), "3");
}
