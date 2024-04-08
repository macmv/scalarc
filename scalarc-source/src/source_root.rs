use std::path::PathBuf;

use la_arena::Idx;

use crate::{SourceDatabase, TargetId};

/// A unique ID for a source root. Each target can have multiple source roots.
/// For example, `src/main` is one source root, and `build.sbt` is another
/// source root.
pub type SourceRootId = Idx<PathBuf>;

pub fn source_root_target(db: &dyn SourceDatabase, id: SourceRootId) -> TargetId {
  let workspace = db.workspace();
  let id = workspace.targets.iter().find(|(_, target)| target.sources.contains(&id)).unwrap().0;
  id
}
