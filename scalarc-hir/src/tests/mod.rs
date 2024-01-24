use scalarc_syntax::SourceFile;

use crate::tree;

pub fn lower(src: &str) -> tree::Block {
  let ast = SourceFile::parse(src);
  let item = crate::lower::lower(ast.tree());
  item
}

#[test]
fn foo() {
  let src = r#"
    val x = 1
    val y = 2
    val z = 3 + 4
  "#;
  let hir = lower(src);
  dbg!(hir);

  panic!();
}
