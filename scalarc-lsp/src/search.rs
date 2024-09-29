//! Converts files and a BSP workspace into FileIds and SourceRootIds.

use std::{collections::HashMap, io, path::Path};

use la_arena::Arena;
use lsp_types::Url;
use scalarc_bsp::types as bsp_types;
use scalarc_source::{FileId, SourceRoot, SourceRootId};

use crate::files::Files;

pub fn workspace_from_sources(
  bsp_targets: bsp_types::WorkspaceBuildTargetsResult,
  bsp_sources: bsp_types::SourcesResult,
  files: &mut Files,
) -> scalarc_source::Workspace {
  let mut targets = Arena::new();
  let mut source_roots = Arena::new();
  let mut name_to_id = HashMap::new();

  for target in &bsp_targets.targets {
    let id = targets.alloc(scalarc_source::TargetData {
      dependencies: vec![],
      bsp_id:       target.id.uri.clone().unwrap(),
      source_roots: vec![],
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
      let root_path = source.uri.to_file_path().unwrap();
      let mut sources = vec![];

      // BSP kinda falls apart right here. Source roots are only real like 20% of the
      // time. The other 80% is "project" source directories that don't exist, target
      // directories, testing directories, and magical directories for the standard
      // library that don't exist at all.
      let _ = discover_sources(&root_path, &mut sources, files);

      let root = SourceRoot { path: root_path.clone(), sources };

      let source_id = source_roots.alloc(root);
      files.create_source_root(source_id, &root_path);
      targets[id].source_roots.push(source_id);
    }
  }

  // Add the standard library.
  let root_path = scalarc_coursier::sources_path();
  let mut sources = vec![];

  discover_sources(&root_path, &mut sources, files).unwrap();
  let root = SourceRoot { path: root_path.clone(), sources };
  let source_id = source_roots.alloc(root);
  files.create_source_root(source_id, &root_path);

  let std_target = targets.alloc(scalarc_source::TargetData {
    dependencies: vec![],
    bsp_id:       Url::parse("file:///std").unwrap(),
    source_roots: vec![],
  });
  targets[std_target].source_roots.push(source_id);

  for (id, target) in targets.iter_mut() {
    if id != std_target {
      target.dependencies.push(std_target);
    }
  }

  files.reindex_all();

  // info!("targets: {:#?}", &targets);

  scalarc_source::Workspace { root: Default::default(), targets, source_roots }
}

fn discover_sources(
  path: impl AsRef<Path>,
  sources: &mut Vec<FileId>,
  files: &mut Files,
) -> io::Result<()> {
  for entry in std::fs::read_dir(path)? {
    let entry = entry?;
    let path = entry.path();
    if path.is_dir() {
      let _ = discover_sources(&path, sources, files);
    } else {
      match files.get_absolute(&path) {
        Some(id) => sources.push(id),
        None => {
          let id = files.create(&path);
          let content = std::fs::read_to_string(&path)?;
          files.write(id, content);
          sources.push(id);
        }
      }
    }
  }

  Ok(())
}
