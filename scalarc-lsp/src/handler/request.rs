use std::{error::Error, path::Path};

use scalarc_analysis::{completion::CompletionKind, FileLocation};
use scalarc_syntax::{Parse, SourceFile, TextSize};

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
    let file_id = snap.files.read().path_to_id(&path);
    let ast = snap.analysis.parse(file_id)?;

    let mut tokens = vec![];

    add_tokens(&mut tokens, ast);

    Ok(Some(lsp_types::SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
      data:      tokens,
      result_id: None,
    })))
  } else {
    Ok(None)
  }
}

fn add_tokens(tokens: &mut Vec<lsp_types::SemanticToken>, ast: Parse<SourceFile>) {
  for item in ast.tree().items() {
    match item {
      scalarc_syntax::ast::Item::ClassDef(o) => {
        if let Some(token) = o.id_token() {
          info!("got token: {:?}", token);
          tokens.push(lsp_types::SemanticToken {
            delta_line:             0,
            delta_start:            token.text_range().start().into(),
            length:                 token.text().len() as u32,
            token_type:             0,
            token_modifiers_bitset: 0,
          });
        }
      }
      _ => {}
    }
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
