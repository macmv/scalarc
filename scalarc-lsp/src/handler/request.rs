use std::{error::Error, iter::once, path::Path};

use lsp_types::SemanticTokenType;
use scalarc_analysis::{
  completion::CompletionKind,
  highlight::{Highlight, HighlightKind},
  FileLocation,
};
use scalarc_source::FileId;
use scalarc_syntax::{TextSize, T};

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
    let highlight = snap.analysis.highlight(file_id)?;

    let tokens = to_semantic_tokens(snap, file_id, &highlight);
    info!("tokens: {:?}", tokens);

    Ok(Some(lsp_types::SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
      data:      tokens,
      result_id: None,
    })))
  } else {
    Ok(None)
  }
}

pub fn handle_goto_definition(
  snap: GlobalStateSnapshot,
  params: lsp_types::GotoDefinitionParams,
) -> Result<Option<lsp_types::GotoDefinitionResponse>, Box<dyn Error>> {
  let pos = file_position(&snap, params.text_document_position_params)?;

  let ast = snap.analysis.parse(pos.file)?;

  let node = ast
    .syntax_node()
    .token_at_offset(pos.index)
    .max_by_key(|token| match token.kind() {
      T![ident] => 10,
      _ => 1,
    })
    .unwrap();

  let definition = match node.kind() {
    T![ident] => {
      let name = node.text().to_string();
      snap.analysis.definition_for_name(pos.file, &name)?
    }
    _ => None,
  };

  if let Some(loc) = definition {
    let files = snap.files.read();
    Ok(Some(lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location::new(
      lsp_types::Url::parse(&format!(
        "file://{}",
        files.workspace.join(files.id_to_path(loc.file)).display()
      ))
      .unwrap(),
      lsp_types::Range::new(lsp_types::Position::new(0, 0), lsp_types::Position::new(0, 0)),
    ))))
  } else {
    Ok(None)
  }
}

pub fn semantic_tokens_legend() -> lsp_types::SemanticTokensLegend {
  fn token_type(kind: HighlightKind) -> SemanticTokenType {
    match kind {
      HighlightKind::Class => SemanticTokenType::new("class"),
      HighlightKind::Function => SemanticTokenType::new("function"),
      HighlightKind::Keyword => SemanticTokenType::new("keyword"),
      HighlightKind::Number => SemanticTokenType::new("number"),
      HighlightKind::Parameter => SemanticTokenType::new("parameter"),
      HighlightKind::Type => SemanticTokenType::new("type"),
      HighlightKind::Variable => SemanticTokenType::new("variable"),
    }
  }

  info!("{:?}", HighlightKind::iter().map(token_type).collect::<Vec<_>>());

  lsp_types::SemanticTokensLegend {
    token_types:     HighlightKind::iter().map(token_type).collect(),
    token_modifiers: vec![],
  }
}

fn to_semantic_tokens(
  snap: GlobalStateSnapshot,
  file: FileId,
  highlight: &Highlight,
) -> Vec<lsp_types::SemanticToken> {
  let mut tokens = Vec::new();

  let mut line = 0;
  let mut col = 0;

  info!("highlight: {:?}", highlight.tokens);

  for h in highlight.tokens.iter() {
    let range = h.range;

    let (l, c) = range_to_line_col(&snap, file, range.start()).unwrap();
    info!("l: {l}, c: {c}");

    let delta_line = l - line;
    if delta_line != 0 {
      col = 0;
    }
    let delta_start = c - col;

    line = l;
    col = c;

    info!(
      "range: {range:?}, line: {line}, col: {col}, delta_line: {delta_line}, delta_start: {delta_start}",
    );

    tokens.push(lsp_types::SemanticToken {
      delta_line,
      delta_start,
      length: (range.end() - range.start()).into(),
      token_type: h.kind as u32,
      token_modifiers_bitset: 0,
    });
  }

  tokens
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

fn range_to_line_col(
  snap: &GlobalStateSnapshot,
  file_id: FileId,
  mut index: TextSize,
) -> Result<(u32, u32), Box<dyn Error>> {
  let files = snap.files.read();
  let file = files.read(file_id);

  for (num, line) in file.lines().enumerate() {
    if u32::from(index) < line.len() as u32 {
      return Ok((num as u32, index.into()));
    }

    index -= TextSize::new(line.len() as u32 + 1);
  }

  Err("position not found".into())
}
