use crate::{database::RootDatabase, FileLocation};
use scalarc_hir::{DefinitionKind, HirDatabase};
use scalarc_parser::{SyntaxKind, T};
use scalarc_source::SourceDatabase;
use scalarc_syntax::{ast, ast::AstNode};

mod field;
mod top_level;

pub struct Completion {
  pub label: String,
  pub kind:  DefinitionKind,
}

pub fn completions(db: &RootDatabase, pos: FileLocation) -> Vec<Completion> {
  completions_inner(db, pos).unwrap_or_default()
}

fn completions_inner(db: &RootDatabase, pos: FileLocation) -> Option<Vec<Completion>> {
  let ast = db.parse(pos.file);
  let node = ast
    .syntax_node()
    .token_at_offset(pos.index)
    .max_by_key(|token| match token.kind() {
      T![ident] => 10,
      SyntaxKind::INT_LIT_KW => 9,

      // Whitespace is always lowest priority.
      T![nl] => 0,

      _ => 1,
    })
    .unwrap();

  let parent = node.parent().unwrap();
  scalarc_syntax::match_ast! {
    match parent {
      ast::FieldExpr(f) => {
        let lhs = f.expr()?;
        // TODO: This is a bit dumb, but not all that dumb.
        let ty = db.type_at(pos.file, lhs.syntax().text_range().end())?;

        field::field_completions(db, pos.file, ty)
      },
      _ => Some(top_level::top_level_completions(db, pos))
    }
  }
}
