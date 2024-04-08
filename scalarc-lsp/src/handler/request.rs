use std::{error::Error, path::Path};

use scalarc_analysis::{completion::CompletionKind, FileLocation};
use scalarc_syntax::TextSize;

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
          kind: Some(match c.kind {
            CompletionKind::Val => lsp_types::CompletionItemKind::VARIABLE,
            CompletionKind::Var => lsp_types::CompletionItemKind::VARIABLE,
            CompletionKind::Class => lsp_types::CompletionItemKind::CLASS,
          }),
          ..Default::default()
        })
        .collect(),
    )))
  } else {
    Ok(None)
  }
}

pub fn handle_semantic_tokens_full(
  snap: GlobalStateSnapshot,
  params: lsp_types::SemanticTokensParams,
) -> Result<Option<lsp_types::SemanticTokensResult>, Box<dyn Error>> {
  if let Some(path) = snap.workspace_path(&params.text_document.uri) {
    let _file_id = snap.files.read().path_to_id(&path);

    Ok(Some(lsp_types::SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
      data:      vec![],
      result_id: None,
    })))
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
  let file_id = files.path_to_id(path);
  let file = files.read(file_id);

  let mut i = 0;
  for (num, line) in file.lines().enumerate() {
    if num as u32 == pos.position.line {
      return Ok(FileLocation { file: file_id, index: TextSize::new(i + pos.position.character) });
    }

    i += line.len() as u32 + 1;
  }

  Err("position not found".into())
}
