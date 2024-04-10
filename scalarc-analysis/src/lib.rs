pub mod completion;
pub mod diagnostic;
pub mod highlight;

mod database;

#[cfg(test)]
mod tests;

#[macro_use]
extern crate log;

use std::{panic::UnwindSafe, sync::Arc};

use completion::Completion;
use diagnostic::Diagnostic;

use database::{LineIndexDatabase, RootDatabase};
use highlight::Highlight;
use line_index::LineIndex;
use salsa::{Cancelled, ParallelDatabase};
use scalarc_hir::{Definition, FileLocation, HirDatabase, Path};
use scalarc_parser::T;
use scalarc_source::{FileId, SourceDatabase, Workspace};

pub struct AnalysisHost {
  db: RootDatabase,
}

/// A snapshot of analysis at a point in time.
pub struct Analysis {
  db: salsa::Snapshot<RootDatabase>,
}

pub type Cancellable<T> = Result<T, Cancelled>;

impl AnalysisHost {
  pub fn new() -> Self {
    let mut db = RootDatabase::default();
    db.set_workspace(Default::default());
    AnalysisHost { db }
  }

  pub fn snapshot(&self) -> Analysis { Analysis { db: self.db.snapshot() } }

  pub fn set_workspace(&mut self, workspace: scalarc_source::Workspace) {
    for (id, root) in workspace.source_roots.iter() {
      for &file in &root.sources {
        self.db.set_file_source_root(file, id);
      }
    }

    self.db.set_workspace(workspace.into());
  }

  pub fn workspace(&self) -> Arc<Workspace> { self.db.workspace() }

  pub fn change(&mut self, change: Change) {
    self.db.set_file_text(change.file, change.text.into());
  }
}

pub struct Change {
  pub file: FileId,
  pub text: String,
}

impl ParallelDatabase for RootDatabase {
  fn snapshot(&self) -> salsa::Snapshot<Self> {
    salsa::Snapshot::new(RootDatabase { storage: self.storage.snapshot() })
  }
}

impl Analysis {
  pub fn completions(&self, cursor: FileLocation) -> Cancellable<Vec<Completion>> {
    self.with_db(|db| completion::completions(db, cursor))
  }

  pub fn diagnostics(&self, file: FileId) -> Cancellable<Vec<Diagnostic>> {
    self.with_db(|db| {
      let ast = db.parse(file);
      ast.errors().iter().map(|err| Diagnostic::from_syntax_error(err)).collect()
    })
  }

  pub fn highlight(&self, file: FileId) -> Cancellable<Highlight> {
    self.with_db(|db| {
      let ast = db.parse(file);
      Highlight::from_ast(ast)
    })
  }

  pub fn parse(
    &self,
    file: FileId,
  ) -> Cancellable<scalarc_syntax::Parse<scalarc_syntax::SourceFile>> {
    self.with_db(|db| db.parse(file))
  }

  pub fn definition_for_name(&self, pos: FileLocation) -> Cancellable<Option<Definition>> {
    self.with_db(|db| {
      let ast = db.parse(pos.file);

      let node = ast
        .syntax_node()
        .token_at_offset(pos.index)
        .max_by_key(|token| match token.kind() {
          T![ident] => 10,
          _ => 1,
        })
        .unwrap();

      match node.kind() {
        T![ident] => {
          let name = node.text().to_string();
          // TODO: Name resolution.
          let path = Path { elems: vec![name.as_str().into()] };

          let source_root = db.file_source_root(pos.file);
          let target = db.source_root_target(source_root);
          let definitions = db.definitions_for_target(target);

          definitions.items.get(&path).cloned()
        }
        _ => None,
      }
    })
  }

  pub fn line_index(&self, file: FileId) -> Cancellable<Arc<LineIndex>> {
    self.with_db(|db| db.line_index(file))
  }

  fn with_db<T>(&self, f: impl FnOnce(&RootDatabase) -> T + UnwindSafe) -> Cancellable<T> {
    Cancelled::catch(|| f(&self.db))
  }
}
