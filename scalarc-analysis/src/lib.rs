mod database;

#[macro_use]
extern crate log;

use std::panic::UnwindSafe;

pub use database::FileId;
use database::RootDatabase;
use salsa::{Cancelled, ParallelDatabase};

use crate::database::SourceDatabase;

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

      for func in ast.tree().fun_decs() {
        info!("func: {:?}", func);
      }

      info!("parsed!");
      vec![]
    })
  }

  fn with_db<T>(&self, f: impl FnOnce(&RootDatabase) -> T + UnwindSafe) -> Cancellable<T> {
    Cancelled::catch(|| f(&self.db))
  }
}