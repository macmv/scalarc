pub mod completion;
pub mod diagnostic;

mod database;

#[cfg(test)]
mod tests;

#[macro_use]
extern crate log;

use std::{panic::UnwindSafe, sync::Arc};

use completion::Completion;
use diagnostic::Diagnostic;

use database::RootDatabase;
use salsa::{Cancelled, ParallelDatabase};
use scalarc_source::{FileId, SourceDatabase, Workspace};
use scalarc_syntax::{Parse, TextSize};

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

pub struct FileLocation {
  pub file:  FileId,
  pub index: TextSize,
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

  pub fn parse(&self, file: FileId) -> Cancellable<Parse<scalarc_syntax::SourceFile>> {
    self.with_db(|db| db.parse(file))
  }

  fn with_db<T>(&self, f: impl FnOnce(&RootDatabase) -> T + UnwindSafe) -> Cancellable<T> {
    Cancelled::catch(|| f(&self.db))
  }
}
