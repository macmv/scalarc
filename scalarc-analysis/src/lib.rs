mod database;
mod diagnostic;

#[macro_use]
extern crate log;

use std::panic::UnwindSafe;

pub use diagnostic::Diagnostic;

use database::RootDatabase;
use salsa::{Cancelled, ParallelDatabase};
use scalarc_source::{FileId, SourceDatabase};

pub struct AnalysisHost {
  db: RootDatabase,
}

/// A snapshot of analysis at a point in time.
pub struct Analysis {
  db: salsa::Snapshot<RootDatabase>,
}

pub type Cancellable<T> = Result<T, Cancelled>;

impl AnalysisHost {
  pub fn new() -> Self { AnalysisHost { db: RootDatabase::default() } }

  pub fn snapshot(&self) -> Analysis { Analysis { db: self.db.snapshot() } }

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
  pub fn completions(&self, file: FileId) -> Cancellable<Vec<()>> {
    self.with_db(|db| {
      info!("parsing...");
      let ast = db.parse(file);

      for item in ast.tree().items() {
        info!("item: {:?}", item);
      }

      info!("parsed!");
      vec![]
    })
  }

  pub fn diagnostics(&self, file: FileId) -> Cancellable<Vec<Diagnostic>> {
    self.with_db(|db| {
      let ast = db.parse(file);
      ast.errors().iter().map(|err| Diagnostic::from_syntax_error(err)).collect()
    })
  }

  fn with_db<T>(&self, f: impl FnOnce(&RootDatabase) -> T + UnwindSafe) -> Cancellable<T> {
    Cancelled::catch(|| f(&self.db))
  }
}
