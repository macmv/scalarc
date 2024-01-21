use std::error::Error;

use crate::global::GlobalStateSnapshot;

pub fn handle_completion(
  snap: GlobalStateSnapshot,
  params: lsp_types::CompletionParams,
) -> Result<Option<lsp_types::CompletionResponse>, Box<dyn Error>> {
  if let Some(path) = snap.workspace_path(&params.text_document_position.text_document.uri) {
    info!("path: {:?}", path);

    let _completions = snap.analysis.completions(scalarc_analysis::FileId::temp_new());

    Ok(Some(lsp_types::CompletionResponse::Array(vec![lsp_types::CompletionItem {
      label: "Hello, World!".to_string(),
      ..Default::default()
    }])))
  } else {
    Ok(None)
  }
}
