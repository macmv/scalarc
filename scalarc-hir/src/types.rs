//! Type inferrence.
//!
//! This should probably be its own crate. Ah well.

use std::fmt;

use crate::{HirDatabase, Path};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{AstNode, SyntaxKind},
  TextSize, T,
};

#[derive(Clone, PartialEq, Eq)]
pub struct Type {
  pub path: Path,
}

impl fmt::Debug for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "Type({})", self) }
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
      SyntaxKind::INT_LIT_KW => 9,

      // Whitespace is always lowest priority.
      T![nl] => 0,

      _ => 1,
    })
    .unwrap();

  match node.kind() {
    T![ident] => {
      let def = db.def_at_index(file_id, pos)?;

      let node = def.node.to_node(&ast.syntax_node());

      match node.kind() {
        SyntaxKind::VAL_DEF => {
          return db.type_at(file_id, node.text_range().end());
        }
        _ => None,
      }
    }

    SyntaxKind::INT_LIT_KW => Some(Type::int()),
    SyntaxKind::FLOAT_LIT_KW => Some(Type::float()),

    SyntaxKind::OPEN_PAREN | SyntaxKind::CLOSE_PAREN => {
      let parent = node.parent()?;

      if scalarc_syntax::ast::TupleExpr::can_cast(parent.kind()) {
        let tup = scalarc_syntax::ast::TupleExpr::cast(parent)?;
        if tup.exprs().count() == 1 {
          db.type_at(file_id, tup.exprs().next()?.syntax().text_range().end())
        } else {
          // TODO: Tuple type
          None
        }
      } else {
        // Parens are the grandchildren of calls, so grab the second parent for this
        // check.
        let parent = parent.parent()?;

        if scalarc_syntax::ast::CallExpr::can_cast(parent.kind()) {
          let call = scalarc_syntax::ast::CallExpr::cast(parent)?;
          let func = call.expr()?;
          let def = db.def_at_index(file_id, func.syntax().text_range().start())?;

          dbg!(&def);

          None
        } else {
          None
        }
      }
    }

    _ => None,
  }
}

impl Type {
  pub fn int() -> Self { Type { path: Path { elems: vec!["scala".into(), "Int".into()] } } }
  pub fn float() -> Self { Type { path: Path { elems: vec!["scala".into(), "Float".into()] } } }
}
