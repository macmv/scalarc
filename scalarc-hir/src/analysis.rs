use crate::{
  tree::{FileArenas, Item, Name},
  Path,
};
use scalarc_source::FileId;
use scalarc_syntax::ast;
use std::collections::HashMap;

pub struct FileAnalyzer {
  pub file_id: FileId,

  // A reverse import map, from the item imported to its resolved path.
  pub imports: HashMap<Name, Path>,

  pub arenas: FileArenas,
}

impl FileAnalyzer {
  pub fn new(file_id: FileId) -> Self {
    FileAnalyzer { file_id, imports: Default::default(), arenas: FileArenas::default() }
  }

  pub fn analyze_item(&mut self, item: ast::Item) {
    match item {
      ast::Item::Import(i) => {
        for expr in i.import_exprs() {
          match expr {
            ast::ImportExpr::Path(p) => {
              let mut path = Path::new();
              for id in p.ids() {
                path.elems.push(Name(id.text().to_string()));
              }

              self.imports.insert(path.elems.last().unwrap().clone(), path);
            }

            ast::ImportExpr::ImportSelectors(_) => {} // TODO
          }
        }
      }

      _ => {}
    }
  }
}
