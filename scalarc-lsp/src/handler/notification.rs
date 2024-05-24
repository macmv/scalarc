use std::error::Error;

use crate::global::GlobalState;
use scalarc_bsp::types::{self as bsp_types, BspRequest};

pub fn handle_open_text_document(
  global: &mut GlobalState,
  params: lsp_types::DidOpenTextDocumentParams,
) -> Result<(), Box<dyn Error>> {
  if let Some(path) = global.workspace_path(&params.text_document.uri) {
    let mut w = global.files.write();
    let file_id = w.create(&path);
    w.write(file_id, params.text_document.text.clone());
  }

  Ok(())
}

pub fn handle_change_text_document(
  global: &mut GlobalState,
  params: lsp_types::DidChangeTextDocumentParams,
) -> Result<(), Box<dyn Error>> {
  if let Some(path) = global.workspace_path(&params.text_document.uri) {
    let file_id = global.files.read().path_to_id(&path);
    let file = global.files.read().read(file_id);

    let new_file = apply_changes(file.clone(), &params.content_changes);

    if file != new_file {
      global.files.write().write(file_id, new_file.clone());
    }
  }

  Ok(())
}

pub fn handle_save_text_document(
  global: &mut GlobalState,
  params: lsp_types::DidSaveTextDocumentParams,
) -> Result<(), Box<dyn Error>> {
  if let Some(path) = global.workspace_path(&params.text_document.uri) {
    let abs_path = global.workspace.join(path);

    if let Some(ref client) = global.bsp_client {
      let workspace = global.analysis_host.workspace();

      let source_roots = workspace
        .source_roots
        .iter()
        .filter(|s| abs_path.starts_with(&s.1.path))
        .collect::<Vec<_>>();
      let targets = workspace
        .targets
        .iter()
        .filter(|t| source_roots.iter().any(|s| t.1.source_roots.contains(&s.0)));

      let id = client.request(bsp_types::BuildTargetCompileRequest {
        targets: targets
          .map(|t| bsp_types::BuildTargetIdentifier { uri: Some(t.1.bsp_id.clone()) })
          .collect(),
        ..Default::default()
      });
      global.bsp_requests.insert(id, bsp_types::BuildTargetCompileRequest::METHOD);
    }
  }

  Ok(())
}

fn apply_changes(
  mut file: String,
  changes: &[lsp_types::TextDocumentContentChangeEvent],
) -> String {
  for change in changes {
    match change.range {
      Some(range) => {
        let start = offset_of(&file, range.start);
        let end = offset_of(&file, range.end);

        file.replace_range(start..end, &change.text);
      }
      None => {
        file.replace_range(.., &change.text);
      }
    }
  }

  file
}

// TODO: Cache this somewhere.
fn offset_of(file: &str, pos: lsp_types::Position) -> usize {
  let mut offset = 0;

  for (i, line) in file.lines().enumerate() {
    if i == pos.line as usize {
      return offset + pos.character as usize;
    }

    offset += line.len() + 1;
  }

  offset
}
