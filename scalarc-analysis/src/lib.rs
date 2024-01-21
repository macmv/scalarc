mod database;

use database::RootDatabase;

pub struct AnalysisHost {
  db: RootDatabase,
}

/// A snapshot of analysis at a point in time.
pub struct Analysis {}

impl AnalysisHost {
  pub fn new() -> Self { AnalysisHost { db: RootDatabase::default() } }
}
