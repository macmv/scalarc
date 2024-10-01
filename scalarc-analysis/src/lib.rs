pub mod completion;
pub mod diagnostic;
pub mod highlight;

mod database;

#[cfg(test)]
mod tests;

#[allow(unused_imports)]
#[macro_use]
extern crate log;

use std::{panic::UnwindSafe, sync::Arc};

use completion::Completion;
use diagnostic::Diagnostic;

use database::{LineIndexDatabase, RootDatabase};
use highlight::Highlight;
use line_index::LineIndex;
use salsa::{Cancelled, ParallelDatabase};
use scalarc_hir::{
  AnyDefinition, FileLocation, FileRange, HirDatabase, LocalDefinition, Reference, Type,
};
use scalarc_source::{FileId, SourceDatabase, Workspace};

pub struct AnalysisHost {
  db: RootDatabase,
}

/// A snapshot of analysis at a point in time.
pub struct Analysis {
  db: salsa::Snapshot<RootDatabase>,
}

pub type Cancellable<T> = Result<T, Cancelled>;

impl Default for AnalysisHost {
  fn default() -> Self { Self::new() }
}

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
        self.db.set_file_source_root(file, Some(id));
      }
    }

    self.db.set_workspace(workspace.into());
  }

  pub fn add_file(&mut self, file: FileId) {
    self.db.set_file_source_root(file, None);
    self.db.set_file_text(file, "".into());
  }

  pub fn workspace(&self) -> Arc<Workspace> { self.db.workspace() }

  pub fn change(&mut self, change: Change) {
    self.db.set_file_text(change.file, change.text.into());
  }

  pub fn index_file(&mut self, file: FileId) {
    // Currently, this just populates the LRU with the complete scope map and HIR
    // ast for the given file. This works nicely, as it is the primary input for
    // completions.
    //
    // However, once type inferrence is more complete, it might be better to infer
    // types as part of indexing.
    self.db.definitions_for_file(file);
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
      ast.errors().iter().map(Diagnostic::from_syntax_error).collect()
    })
  }

  pub fn highlight(&self, file: FileId) -> Cancellable<Highlight> {
    self.with_db(|db| Highlight::from_ast(db, file))
  }

  pub fn parse(
    &self,
    file: FileId,
  ) -> Cancellable<scalarc_syntax::Parse<scalarc_syntax::SourceFile>> {
    self.with_db(|db| db.parse(file))
  }

  pub fn definition_for_name(
    &self,
    pos: FileLocation,
  ) -> Cancellable<Option<(AnyDefinition, FileRange)>> {
    self.with_db(|db| {
      db.def_at_index(pos.file, pos.index).map(|def| match def {
        scalarc_hir::AnyDefinition::Local(ref d) => {
          let ast = db.parse(pos.file);
          let file = d.block_id.file_id;
          let item_ptr = db.hir_source_map_for_scope(d.block_id).stmt_syntax(d.stmt_id).unwrap();
          let item = item_ptr.to_node(&ast);
          (def, FileRange { file, range: item.text_range() })
        }
        scalarc_hir::AnyDefinition::Global(ref d) => {
          let file = d.file_id;
          let item = db.ast_id_map(d.file_id).get_erased(d.ast_id);
          (def, FileRange { file, range: item.text_range() })
        }
      })
    })
  }

  pub fn references_for_name(&self, pos: FileLocation) -> Cancellable<Vec<Reference>> {
    self.with_db(|db| db.references_to(pos.file, pos.index))
  }

  pub fn type_at(&self, pos: FileLocation) -> Cancellable<Option<Type>> {
    self.with_db(|db| db.type_at(pos.file, pos.index))
  }

  pub fn line_index(&self, file: FileId) -> Cancellable<Arc<LineIndex>> {
    self.with_db(|db| db.line_index(file))
  }

  fn with_db<T>(&self, f: impl FnOnce(&RootDatabase) -> T + UnwindSafe) -> Cancellable<T> {
    Cancelled::catch(|| f(&self.db))
  }
}
