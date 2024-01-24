use crate::tree;
use scalarc_source::{FileId, SourceDatabase};
use scalarc_syntax::{Parse, SourceFile};
use std::fmt;

#[salsa::database(
  scalarc_source::SourceDatabaseStorage,
  crate::InternDatabaseStorage,
  crate::HirDatabaseStorage
)]
pub(crate) struct TestDB {
  storage: salsa::Storage<TestDB>,
}

impl Default for TestDB {
  fn default() -> Self { Self { storage: Default::default() } }
}

impl salsa::Database for TestDB {}

impl fmt::Debug for TestDB {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { f.debug_struct("TestDB").finish() }
}

#[test]
fn foo() {
  let src = r#"
    val x = 1
    val y = 2
    val z = 3 + 4
  "#;

  // Make a new DB, and stick a file in it.
  let mut db = TestDB::default();
  let file = FileId::temp_new();
  db.set_file_text(file, src.into());

  // Lower the file into a tree.
  let hir = crate::lower::lower(&db, file);

  // Grab a Val from the HIR.
  let crate::tree::Item::Val(v) = &hir.items[0] else { panic!() };

  // Look it up in the AST.
  let ptr = v.id.get(&db, file);
  let ast = db.parse(file);
  let node = ptr.to_node(&ast.syntax_node());

  dbg!(&node);

  panic!();
}
