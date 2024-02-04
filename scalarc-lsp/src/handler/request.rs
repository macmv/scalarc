use std::{error::Error, path::Path};

use scalarc_analysis::FileLocation;

use crate::global::GlobalStateSnapshot;

pub fn handle_completion(
  snap: GlobalStateSnapshot,
  params: lsp_types::CompletionParams,
) -> Result<Option<lsp_types::CompletionResponse>, Box<dyn Error>> {
  if let Some(path) = snap.workspace_path(&params.text_document_position.text_document.uri) {
    info!("path: {:?}", path);

    let completions =
      snap.analysis.completions(file_position(&snap, params.text_document_position)?)?;

    Ok(Some(lsp_types::CompletionResponse::Array(
      completions
        .into_iter()
        .map(|c| lsp_types::CompletionItem {
          label: c.label,
          kind: Some(lsp_types::CompletionItemKind::CLASS),
          ..Default::default()
        })
        .collect(),
    )))
  } else {
    Ok(None)
  }
}

fn file_position(
  snap: &GlobalStateSnapshot,
  pos: lsp_types::TextDocumentPositionParams,
) -> Result<FileLocation, Box<dyn Error>> {
  let files = snap.files.read();

  let path = Path::new(pos.text_document.uri.path());

  let file = files.read(path);
  let file_id = snap.files.read().path_to_id(path);

  let mut i = 0;
  for (num, line) in file.lines().enumerate() {
    if num == pos.position.line as usize {
      return Ok(FileLocation { file: file_id, index: i });
    }

    i += line.len() + 1;
  }

  Err("position not found".into())
}
