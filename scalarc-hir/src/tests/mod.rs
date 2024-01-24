use crate::tree;
use scalarc_source::{FileId, SourceDatabase};
use scalarc_syntax::SourceFile;
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

pub fn lower(src: &str) -> tree::Package {
  let ast = SourceFile::parse(src);
  let mut db = TestDB::default();
  db.set_file_text(FileId::temp_new(), src.into());
  let item = crate::lower::lower(&db, FileId::temp_new(), ast.tree());
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
