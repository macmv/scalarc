use scalarc_source::SourceDatabase;
use scalarc_syntax::{Parse, SourceFile};

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
  info!("token: {:?}", token);

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
