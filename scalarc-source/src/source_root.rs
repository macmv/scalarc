use crate::{FileId, SourceDatabase, SourceRootId, TargetId};

pub fn source_root_target(db: &dyn SourceDatabase, id: SourceRootId) -> TargetId {
  let workspace = db.workspace();
  let id =
    workspace.targets.iter().find(|(_, target)| target.source_roots.contains(&id)).unwrap().0;
  id
}

pub fn file_target(db: &dyn SourceDatabase, file_id: FileId) -> Option<TargetId> {
  let source_root = db.file_source_root(file_id)?;
  Some(db.source_root_target(source_root))
}
