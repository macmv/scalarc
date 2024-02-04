use std::error::Error;

use scalarc_source::FileId;

use crate::global::GlobalStateSnapshot;

pub fn handle_completion(
  snap: GlobalStateSnapshot,
  params: lsp_types::CompletionParams,
) -> Result<Option<lsp_types::CompletionResponse>, Box<dyn Error>> {
  if let Some(path) = snap.workspace_path(&params.text_document_position.text_document.uri) {
    info!("path: {:?}", path);

    let completions = snap.analysis.completions(FileId::temp_new())?;

    Ok(Some(lsp_types::CompletionResponse::Array(
      completions
        .into_iter()
        .map(|c| lsp_types::CompletionItem { label: c.label, ..Default::default() })
        .collect(),
    )))
  } else {
    Ok(None)
  }
}
