use std::collections::HashSet;

use scalarc_source::SourceDatabase;
use scalarc_syntax::{
  ast::AstNode,
  node::{SyntaxNode, SyntaxToken},
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
  let scopes = scopes_for(&token.left_biased().unwrap().parent().unwrap());

  let mut names = HashSet::new();
  for scope in scopes {
    for decl in scope.declarations {
      if names.insert(decl.clone()) {
        completions.push(Completion { label: decl });
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
fn scopes_for(token: &SyntaxNode) -> Vec<Scope> {
  let mut scopes = vec![];

  let mut tok = token.clone();

  loop {
    scopes.push(collect_scope(&tok));

    if let Some(parent) = tok.parent() {
      tok = parent;
    } else {
      break;
    }
  }

  scopes
}

fn collect_scope(tok: &SyntaxNode) -> Scope {
  let mut declarations = vec![];

  for node in tok.children() {
    match node.kind() {
      scalarc_parser::SyntaxKind::VAL_DEF => {
        let node = scalarc_syntax::ast::ValDef::cast(node).unwrap();
        if let Some(id) = node.id_token() {
          declarations.push(id.text().into());
        }
      }
      _ => {}
    }
  }

  Scope { declarations }
}
