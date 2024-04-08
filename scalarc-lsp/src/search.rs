//! Converts files and a BSP workspace into FileIds and SourceRootIds.

use std::{collections::HashMap, path::PathBuf};

use la_arena::Arena;
use scalarc_bsp::types as bsp_types;
use scalarc_source::SourceRootId;

// TODO: This structure is wrong, the same path can correspond to multiple
// source roots. Need to figure out what the actual model is here.
pub struct FileToSourceMap {
  directories: HashMap<PathBuf, SourceRootId>,
}

impl FileToSourceMap {
  fn new() -> Self { FileToSourceMap { directories: Default::default() } }

  fn insert(&mut self, file: PathBuf, source: SourceRootId) {
    if let Some(existing) = self.directories.get(&file) {
      panic!(
        "file {:?} already has source root {:?}, but we tried to insert {:?}",
        file, existing, source
      );
    }
    self.directories.insert(file, source);
  }
}

pub fn workspace_from_sources(
  bsp_targets: bsp_types::WorkspaceBuildTargetsResult,
  bsp_sources: bsp_types::SourcesResult,
) -> scalarc_source::Workspace {
  let mut targets = Arena::new();
  let mut sources = Arena::new();
  let mut name_to_id = HashMap::new();
  let mut file_to_source_map = FileToSourceMap::new();

  for target in &bsp_targets.targets {
    let id = targets.alloc(scalarc_source::TargetData {
      dependencies: vec![],
      bsp_id:       target.id.uri.clone().unwrap(),
      sources:      vec![],
    });

    name_to_id.insert(target.id.uri.clone().unwrap(), id);
  }

  // Fill in dependencies
  for target in bsp_targets.targets {
    let id = name_to_id[&target.id.uri.clone().unwrap()];
    targets[id].dependencies = target
      .dependencies
      .iter()
      .filter_map(|t| {
        let id = name_to_id.get(t.uri.as_ref().unwrap()).copied();
        if id.is_none() {
          warn!("unknown dependency {:?}", t.uri);
        }
        id
      })
      .collect();
  }

  for bsp_sources in bsp_sources.items {
    let id = name_to_id[&bsp_sources.target.uri.clone().unwrap()];
    for source in bsp_sources.sources {
      let source_id = sources.alloc(source.uri.to_file_path().unwrap());
      file_to_source_map.insert(source.uri.to_file_path().unwrap(), source_id);
      targets[id].sources.push(source_id);
    }
  }

  info!("targets: {:#?}", &targets);

  scalarc_source::Workspace { root: Default::default(), targets }
}
