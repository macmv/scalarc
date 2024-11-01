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
  AnyDefinition, ClassKind, DefinitionKey, FileLocation, FileRange, GlobalDefinition,
  GlobalDefinitionKind, HirDatabase, HirDefinitionId, Reference, Type,
};
use scalarc_parser::{SyntaxKind, T};
use scalarc_source::{FileId, SourceDatabase, TargetId, Workspace};

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
      let def = db.def_at_index(pos.file, pos.index)?;
      match def {
        scalarc_hir::AnyDefinition::Hir(ref d) => {
          let file = d.block_id.file_id;
          let ast = db.parse(file);
          let source_map = db.hir_source_map_for_block(d.block_id);
          match d.id {
            HirDefinitionId::Stmt(s) => {
              let item = source_map.stmt_syntax(s).unwrap().to_node(&ast);
              Some((def, FileRange { file, range: item.text_range() }))
            }
            HirDefinitionId::Param(s) => {
              let item = source_map.param_syntax(s).unwrap().to_node(&ast.syntax_node());
              Some((def, FileRange { file, range: item.text_range() }))
            }
            HirDefinitionId::Pattern(s) => {
              let item = source_map.pattern_syntax(s).unwrap().to_node(&ast);
              Some((def, FileRange { file, range: item.text_range() }))
            }
            HirDefinitionId::Import(id) => {
              let import = &db.hir_ast_for_block(d.block_id).imports[id];
              let target = db.file_target(file).unwrap();
              let def = prioritize_definitions(&db, pos, target, &import.path)?;

              let file = def.file_id;
              let item = db.ast_id_map(def.file_id).get_erased(def.ast_id);

              Some((AnyDefinition::Global(def), FileRange { file, range: item.text_range() }))
            }
          }
        }
        scalarc_hir::AnyDefinition::Global(ref d) => {
          let file = d.file_id;
          let item = db.ast_id_map(d.file_id).get_erased(d.ast_id);
          Some((def, FileRange { file, range: item.text_range() }))
        }
      }
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

// Picking the definition is a bit tricky, and metals in particular doesn't do
// this very well. So, I've decided the following priority order:
// - If the current name is part of a call (`Seq|()`), then jump to the apply
//   impl.
//   - If there is a specific apply impl, jump there.
//   - If none of the apply impls match for a case class, jump to the definition
//     of the case class.
//   - If none of the apply impls match for a normal class, jump to the object
//     definition.
//   - Otherwise, jump to the definition of the case class (even if the params
//     don't match).
// - If the current name is anything else, jump to the object first.
// - If the object does not resolve, jump to the instance instead.
fn prioritize_definitions(
  db: &RootDatabase,
  cursor: FileLocation,
  target: TargetId,
  path: &scalarc_hir::Path,
) -> Option<GlobalDefinition> {
  let ast = db.parse(cursor.file);

  let token = ast
    .syntax_node()
    .token_at_offset(cursor.index)
    .max_by_key(|token| match token.kind() {
      T![ident] => 10,
      SyntaxKind::INT_LIT_KW => 9,

      // Whitespace is always lowest priority.
      T![nl] => 0,

      _ => 1,
    })
    .unwrap();

  let parent = token.parent().unwrap();
  match parent.kind() {
    SyntaxKind::IDENT_EXPR => {
      let parent2 = parent.parent().unwrap();
      match parent2.kind() {
        // TODO: Handle field exprs.
        SyntaxKind::CALL_EXPR => {
          let Some(class) = db.definition_for_key(target, DefinitionKey::Instance(path.clone()))
          else {
            return db.definition_for_key(target, DefinitionKey::Object(path.clone()));
          };

          match class.kind {
            GlobalDefinitionKind::Class(_, ClassKind::Case) => Some(class),
            GlobalDefinitionKind::Class(_, _) => {
              db.definition_for_key(target, DefinitionKey::Object(path.clone()))
            }
            _ => None,
          }
        }
        _ => None,
      }
    }

    SyntaxKind::NEW_EXPR => db.definition_for_key(target, DefinitionKey::Instance(path.clone())),

    _ => db.definition_for_key(target, DefinitionKey::Object(path.clone())),
  }
}
