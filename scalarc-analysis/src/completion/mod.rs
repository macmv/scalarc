use crate::FileLocation;
use scalarc_hir::{DefinitionKind, HirDatabase, Type};
use scalarc_parser::{SyntaxKind, T};
use scalarc_source::TargetId;
use scalarc_syntax::{ast, ast::AstNode};

mod field;
mod top_level;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Completion {
  pub label: String,
  pub kind:  DefinitionKind,
}

#[salsa::query_group(CompletionsDatabaseStorage)]
pub trait CompletionsDatabase: HirDatabase {
  fn completions(&self, pos: FileLocation) -> Vec<Completion>;

  #[salsa::invoke(field::field_completions)]
  fn field_completions(&self, target: TargetId, ty: Type) -> Vec<Completion>;

  #[salsa::invoke(top_level::top_level_completions)]
  fn top_level_completions(&self, pos: FileLocation) -> Vec<Completion>;
}

fn completions(db: &dyn CompletionsDatabase, pos: FileLocation) -> Vec<Completion> {
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
        let Some(lhs) = f.expr() else  { return vec![] };
        // TODO: This is a bit dumb, but not all that dumb.
        let Some(ty) = db.type_at(pos.file, lhs.syntax().text_range().end()) else { return vec![] };

        let target = db.file_target(pos.file).unwrap();
        db.field_completions(target, ty)
      },
      _ => db.top_level_completions(pos)
    }
  }
}
