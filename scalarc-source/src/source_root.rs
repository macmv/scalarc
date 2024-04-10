use crate::{SourceDatabase, SourceRootId, TargetId};

pub fn source_root_target(db: &dyn SourceDatabase, id: SourceRootId) -> TargetId {
  let workspace = db.workspace();
  let id =
    workspace.targets.iter().find(|(_, target)| target.source_roots.contains(&id)).unwrap().0;
  id
}
