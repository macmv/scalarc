use crate::tree::Item;
use scalarc_source::{FileId, SourceDatabase};
use std::{fmt, sync::Mutex};

mod incremental;

#[salsa::database(scalarc_source::SourceDatabaseStorage, crate::HirDatabaseStorage)]
pub(crate) struct TestDB {
  storage: salsa::Storage<TestDB>,
  events:  Mutex<Option<Vec<salsa::Event>>>,
}

impl Default for TestDB {
  fn default() -> Self { Self { storage: Default::default(), events: Some(vec![]).into() } }
}

impl salsa::Database for TestDB {
  fn salsa_event(&self, event: salsa::Event) {
    let mut events = self.events.lock().unwrap();
    if let Some(events) = &mut *events {
      events.push(event);
    }
  }
}

impl fmt::Debug for TestDB {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { f.debug_struct("TestDB").finish() }
}

impl TestDB {
  pub(crate) fn log(&self, f: impl FnOnce()) -> Vec<salsa::Event> {
    *self.events.lock().unwrap() = Some(Vec::new());
    f();
    self.events.lock().unwrap().take().unwrap()
  }

  pub(crate) fn log_executed(&self, f: impl FnOnce()) -> Vec<String> {
    let events = self.log(f);
    events
      .into_iter()
      .filter_map(|e| match e.kind {
        // This is pretty horrible, but `Debug` is the only way to inspect
        // QueryDescriptor at the moment.
        salsa::EventKind::WillExecute { database_key } => {
          Some(format!("{:?}", database_key.debug(self)))
        }
        _ => None,
      })
      .collect()
  }
}

#[test]
fn body() {
  let mut db = TestDB::default();
  let file = FileId::temp_new();
  db.set_file_text(file, "def foo = 2 * 3".into());
  // let package = db.file_package(file);
  // let Item::Def(def) = package.items[0] else { panic!() };

  // assert_eq!(package.arenas.def[def].name, "foo".into());
}
