use crate::{database::RootDatabase, FileLocation};
use scalarc_hir::{DefinitionKind, HirDatabase};
use scalarc_parser::{SyntaxKind, T};
use scalarc_source::SourceDatabase;
use scalarc_syntax::{ast, ast::AstNode};

mod field;
mod top_level;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Completion {
  pub label: String,
  pub kind:  DefinitionKind,
}

struct Completer<'a> {
  db: &'a RootDatabase,
}

pub fn completions(db: &RootDatabase, pos: FileLocation) -> Vec<Completion> {
  let completer = Completer { db };

  completer.completions(pos)
}

impl Completer<'_> {
  fn completions(&self, pos: FileLocation) -> Vec<Completion> {
    let ast = self.db.parse(pos.file);
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
          let Some(ty) = self.db.type_at(pos.file, lhs.syntax().text_range().end()) else { return vec![] };

          let target = self.db.file_target(pos.file).unwrap();
          self.field_completions(target, ty)
        },
        _ => self.top_level_completions(pos)
      }
    }
  }
}
