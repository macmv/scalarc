use std::collections::HashSet;

use super::{Completer, Completion, CompletionKind};
use scalarc_hir::{hir, DefinitionKey, GlobalDefinitionKind, HirDatabase, InFileExt};
use scalarc_parser::{SyntaxKind, T};
use scalarc_source::SourceDatabase;
use scalarc_syntax::{
  ast::{self, AstNode},
  match_ast,
  node::SyntaxNode,
  SyntaxNodePtr,
};

impl Completer<'_> {
  pub fn top_level_completions(&self) -> Vec<Completion> {
    let Some(source_root) = self.db.file_source_root(self.cursor.file) else { return vec![] };
    let target = self.db.source_root_target(source_root);

    let mut definitions = vec![];
    for target in self.db.workspace().all_dependencies(target) {
      definitions.extend(self.db.definitions_for_target(target).items.into_iter().filter(
        |(_, def)| match def.kind {
          GlobalDefinitionKind::Object(_) => true,
          _ => false,
        },
      ));
    }

    let mut completions = definitions
      .into_iter()
      .map(|(key, def)| Completion {
        label: match key {
          DefinitionKey::Instance(_) => unreachable!(),
          DefinitionKey::Object(mut p) => p.elems.pop().unwrap().into_string(),
        },
        kind:  CompletionKind::Global(def.kind),
      })
      .collect::<Vec<_>>();

    let ast = self.db.parse(self.cursor.file);

    let token = ast
      .syntax_node()
      .token_at_offset(self.cursor.index)
      .max_by_key(|token| match token.kind() {
        T![ident] => 10,
        SyntaxKind::INT_LIT_KW => 9,

        // Whitespace is always lowest priority.
        T![nl] => 0,

        _ => 1,
      })
      .unwrap();

    let Some(mut n) = token.parent() else { return completions };
    loop {
      match_ast! {
        match n {
          ast::Expr(_) => {
            self.collect_block_completions(&n, &mut completions);
            break;
          },
          ast::ItemBody(_) => {
            self.collect_block_completions( &n, &mut completions);
            break;
          },
          _ => n = match n.parent() {
            Some(p) => p,
            None => break,
          },
        }
      }
    }

    completions
  }

  fn collect_block_completions(&self, node: &SyntaxNode, completions: &mut Vec<Completion>) {
    let ast = self.db.parse(self.cursor.file);

    let block_id = self.db.block_for_node(SyntaxNodePtr::new(node).in_file(self.cursor.file));
    let (block, source_map) = self.db.hir_ast_with_source_for_block(block_id);

    let mut names = HashSet::new();
    for item in &block.items {
      match block.stmts[*item] {
        hir::Stmt::Binding(ref binding) => {
          let node = source_map.stmt_syntax(*item).unwrap();
          // Recursive vals and defs exist, so we check if the start is greater than
          // the cursor.
          if node.to_node(&ast).text_range().start() > self.cursor.index {
            continue;
          }

          if names.insert(binding.name.clone()) {
            completions.push(Completion {
              label: binding.name.as_str().into(),
              kind:  CompletionKind::Hir(scalarc_hir::HirDefinitionKind::Val(None)),
            });
          }
        }
        _ => {}
      }
    }
  }
}
