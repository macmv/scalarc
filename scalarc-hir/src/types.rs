//! Type inferrence.
//!
//! This should probably be its own crate. Ah well.

use std::fmt;

use crate::{HirDatabase, Path};
use scalarc_source::FileId;
use scalarc_syntax::{ast::SyntaxKind, TextSize, T};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
  pub path: Path,
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, elem) in self.path.elems.iter().enumerate() {
      if i != 0 {
        write!(f, ".")?;
      }
      write!(f, "{}", elem.as_str())?;
    }

    Ok(())
  }
}

pub fn type_at(db: &dyn HirDatabase, file_id: FileId, pos: TextSize) -> Option<Type> {
  let ast = db.parse(file_id);

  // FIXME: Dedupe with `scope`
  let node = ast
    .syntax_node()
    .token_at_offset(pos)
    .max_by_key(|token| match token.kind() {
      T![ident] => 10,
      _ => 1,
    })
    .unwrap();

  dbg!(&node);

  match node.kind() {
    T![ident] => {
      let def = db.def_at_index(file_id, pos)?;

      Some(Type { path: Path { elems: vec![def.name] } })
    }

    SyntaxKind::INT_LIT_KW => Some(Type::int()),
    SyntaxKind::FLOAT_LIT_KW => Some(Type::float()),

    _ => None,
  }
}

impl Type {
  pub fn int() -> Self { Type { path: Path { elems: vec!["scala".into(), "Int".into()] } } }
  pub fn float() -> Self { Type { path: Path { elems: vec!["scala".into(), "Float".into()] } } }
}
