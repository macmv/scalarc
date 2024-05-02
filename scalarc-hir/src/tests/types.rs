use scalarc_source::FileId;
use scalarc_syntax::TextSize;
use scalarc_test::{expect, Expect};

use crate::HirDatabase;

use super::new_db;

fn type_at(src: &str, expected: Expect) {
  let cursor = src.find("@@").unwrap();
  let src = format!("{}{}", &src[..cursor], &src[cursor + 2..]);

  let db = new_db(&src);
  let actual = db.type_at(FileId::temp_new(), TextSize::from(cursor as u32));

  let actual = match actual {
    Some(ty) => ty.to_string(),
    None => "no type".to_string(),
  };

  expected.assert_eq(&actual);
}

#[test]
fn simple_type_at() {
  type_at("@@3", expect![@"scala.Int"]);
  type_at("3@@", expect![@"scala.Int"]);
  type_at("3.2@@", expect![@"scala.Float"]);

  type_at("@@ 3", expect![@"no type"]);
}
