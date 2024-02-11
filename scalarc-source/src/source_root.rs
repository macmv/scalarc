use crate::{SourceDatabase, TargetId};

/// A unique ID for a source root. Each target can have multiple source roots.
/// For example, `src/main` is one source root, and `build.sbt` is another
/// source root.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceRootId(pub u32);

pub fn source_root_target(db: &dyn SourceDatabase, id: SourceRootId) -> Option<TargetId> {
  let workspace = db.workspace();
  let target =
    workspace.targets.iter().find(|(_, target)| target.source_root == id).map(|(id, _)| id);
  target
}
