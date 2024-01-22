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
