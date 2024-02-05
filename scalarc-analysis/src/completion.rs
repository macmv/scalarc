use std::collections::HashSet;

use scalarc_source::SourceDatabase;
use scalarc_syntax::{
  ast::AstNode,
  node::{NodeOrToken, SyntaxNode, SyntaxToken},
  Parse, SourceFile,
};

use crate::{database::RootDatabase, FileLocation};

pub struct Completion {
  pub label: String,
}

pub fn completions(db: &RootDatabase, cursor: FileLocation) -> Vec<Completion> {
  let mut completions = vec![];

  info!("finding completions...");

  let ast = db.parse(cursor.file);
  add_imports(&mut completions, &ast);

  let token = ast.syntax_node().token_at_offset(cursor.index);
  let token = token.left_biased().unwrap();
  let scopes = scopes_for(&token);

  let mut names = HashSet::new();
  for scope in scopes {
    for name in scope.declarations {
      if names.insert(name.clone()) {
        completions.push(Completion { label: name });
      }
    }
  }

  info!("got {} completions!", completions.len());

  completions
}

fn add_imports(completions: &mut Vec<Completion>, ast: &Parse<SourceFile>) {
  for item in ast.tree().items() {
    match item {
      scalarc_syntax::ast::Item::Import(i) => {
        for expr in i.import_exprs() {
          match expr {
            scalarc_syntax::ast::ImportExpr::Path(p) => {
              if let Some(name) = p.ids().last() {
                completions.push(Completion { label: name.text().into() });
              }
            }
            scalarc_syntax::ast::ImportExpr::ImportSelectors(selectors) => {
              for selector in selectors.import_selectors() {
                match selector {
                  scalarc_syntax::ast::ImportSelector::ImportSelectorId(ident) => {
                    if let Some(id) = ident.id_token() {
                      completions.push(Completion { label: id.text().into() });
                    }
                  }
                  _ => {}
                }
              }
            }
          }
        }
      }
      _ => {}
    }
  }
}

struct Scope {
  declarations: Vec<String>,
}

// Returns the scopes around the given token. The first scope is the innermost.
fn scopes_for(token: &SyntaxToken) -> Vec<Scope> {
  let mut scopes = vec![];

  let mut n: NodeOrToken = token.clone().into();

  loop {
    scopes.push(collect_scope(&n));

    if let Some(parent) = n.parent() {
      n = parent.into();
    } else {
      break;
    }
  }

  scopes
}

fn collect_scope(t: &NodeOrToken) -> Scope {
  let mut declarations = vec![];

  for n in iter_prev_siblings(t) {
    match n.kind() {
      scalarc_parser::SyntaxKind::VAL_DEF => {
        let n = scalarc_syntax::ast::ValDef::cast(n).unwrap();
        if let Some(id) = n.id_token() {
          declarations.push(id.text().into());
        }
      }
      _ => {}
    }
  }

  declarations.reverse();

  Scope { declarations }
}

fn iter_prev_siblings(t: &NodeOrToken) -> impl Iterator<Item = SyntaxNode> {
  struct Iter {
    prev: NodeOrToken,
  }

  impl Iterator for Iter {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<SyntaxNode> {
      loop {
        let next = self.prev.prev_sibling_or_token()?;
        self.prev = next.clone();
        if let Some(node) = next.as_node() {
          return Some(node.clone());
        }
      }
    }
  }

  Iter { prev: t.clone() }
}
