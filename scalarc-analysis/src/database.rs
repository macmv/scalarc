use std::fmt;

use scalarc_source::SourceDatabaseStorage;

#[salsa::database(SourceDatabaseStorage)]
#[derive(Default)]
pub struct RootDatabase {
  pub(crate) storage: salsa::Storage<Self>,
}
impl salsa::Database for RootDatabase {}

impl fmt::Debug for RootDatabase {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("RootDatabase").finish()
  }
}
