use la_arena::Arena;
use scalarc_source::{FileId, SourceDatabase, SourceRoot, TargetData};
use std::{
  fmt,
  path::PathBuf,
  sync::{Arc, Mutex},
};

mod incremental;
mod scope;
mod types;

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

pub fn new_db(content: &str) -> TestDB {
  let mut db = TestDB::default();
  let file = FileId::temp_new();
  db.set_file_text(file, content.into());

  // Build a workspace with a single source root, with a single file in it.
  let mut targets = Arena::new();
  let mut source_roots = Arena::new();

  let source_root = source_roots.alloc(SourceRoot { path: PathBuf::new(), sources: vec![file] });

  targets.alloc(TargetData {
    dependencies: vec![],
    bsp_id:       "file:///".parse().unwrap(),
    source_roots: vec![source_root],
  });

  db.set_file_source_root(file, Some(source_root));

  let workspace = scalarc_source::Workspace { root: Default::default(), targets, source_roots };
  db.set_workspace(Arc::new(workspace));

  db
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
