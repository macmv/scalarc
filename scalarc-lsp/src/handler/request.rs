use std::{error::Error, path::Path};

use lsp_types::SemanticTokenType;
use scalarc_analysis::highlight::{Highlight, HighlightKind};
use scalarc_hir::{DefinitionKind, FileLocation, GlobalDefinition, LocalDefinition};
use scalarc_source::FileId;
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
        .map(|c| {
          let (kind, detail) = match c.kind {
            DefinitionKind::Global(g) => match g {
              GlobalDefinition::Class => (lsp_types::CompletionItemKind::CLASS, None),
              GlobalDefinition::Object => (lsp_types::CompletionItemKind::CLASS, None),
            },
            DefinitionKind::Local(l) => match l {
              LocalDefinition::Val(ty) => {
                (lsp_types::CompletionItemKind::VARIABLE, ty.map(|t| t.to_string()))
              }
              LocalDefinition::Var => (lsp_types::CompletionItemKind::VARIABLE, None),
              LocalDefinition::Parameter => (lsp_types::CompletionItemKind::VARIABLE, None),
              LocalDefinition::Def(sig) => {
                (lsp_types::CompletionItemKind::FUNCTION, Some(sig.to_string()))
              }
            },
          };

          lsp_types::CompletionItem {
            label: c.label,
            label_details: detail.map(|d| lsp_types::CompletionItemLabelDetails {
              detail: Some(d),
              ..Default::default()
            }),
            kind: Some(kind),
            ..Default::default()
          }
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

    let tokens = to_semantic_tokens(snap, file_id, &highlight)?;
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
  let definition = snap.analysis.definition_for_name(pos)?;

  if let Some(def) = definition {
    let files = snap.files.read();

    let line_index = snap.analysis.line_index(def.pos.file)?;
    let start = line_index.line_col(def.pos.range.start());
    let end = line_index.line_col(def.pos.range.end());

    let start = lsp_types::Position { line: start.line, character: start.col };
    let end = lsp_types::Position { line: end.line, character: end.col };

    Ok(Some(lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location::new(
      lsp_types::Url::parse(&format!(
        "file://{}",
        files.workspace.join(files.id_to_path(def.pos.file)).display()
      ))
      .unwrap(),
      lsp_types::Range { start, end },
    ))))
  } else {
    Ok(None)
  }
}

pub fn handle_document_highlight(
  snap: GlobalStateSnapshot,
  params: lsp_types::DocumentHighlightParams,
) -> Result<Option<Vec<lsp_types::DocumentHighlight>>, Box<dyn Error>> {
  let pos = file_position(&snap, params.text_document_position_params)?;
  let line_index = snap.analysis.line_index(pos.file)?;
  let definition = snap.analysis.definition_for_name(pos)?;
  let refs = snap.analysis.references_for_name(pos)?;

  if let Some(def) = definition {
    if def.pos.file != pos.file {
      return Ok(None);
    }

    let start = line_index.line_col(def.pos.range.start());
    let end = line_index.line_col(def.pos.range.end());

    let start = lsp_types::Position { line: start.line, character: start.col };
    let end = lsp_types::Position { line: end.line, character: end.col };

    let def_highlight = lsp_types::DocumentHighlight {
      range: lsp_types::Range { start, end },
      kind:  Some(lsp_types::DocumentHighlightKind::WRITE),
    };

    let refs_highlight = refs.into_iter().map(|r| {
      let start = line_index.line_col(r.pos.range.start());
      let end = line_index.line_col(r.pos.range.end());

      let start = lsp_types::Position { line: start.line, character: start.col };
      let end = lsp_types::Position { line: end.line, character: end.col };

      lsp_types::DocumentHighlight {
        range: lsp_types::Range { start, end },
        kind:  Some(lsp_types::DocumentHighlightKind::READ),
      }
    });

    Ok(Some([def_highlight].into_iter().chain(refs_highlight).collect()))
  } else {
    Ok(None)
  }
}

pub fn handle_hover(
  snap: GlobalStateSnapshot,
  params: lsp_types::HoverParams,
) -> Result<Option<lsp_types::Hover>, Box<dyn Error>> {
  let pos = file_position(&snap, params.text_document_position_params)?;
  let line_index = snap.analysis.line_index(pos.file)?;
  let def = snap.analysis.definition_for_name(pos)?;
  let ty = snap.analysis.type_at(pos)?;

  if let Some(ty) = ty {
    let range = def.map(|d| {
      let start = line_index.line_col(d.pos.range.start());
      let end = line_index.line_col(d.pos.range.end());

      let start = lsp_types::Position { line: start.line, character: start.col };
      let end = lsp_types::Position { line: end.line, character: end.col };

      lsp_types::Range { start, end }
    });

    Ok(Some(lsp_types::Hover {
      range,
      contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(ty.to_string())),
    }))
  } else {
    Ok(None)
  }
}

pub fn semantic_tokens_legend() -> lsp_types::SemanticTokensLegend {
  fn token_type(kind: HighlightKind) -> SemanticTokenType {
    match kind {
      HighlightKind::Class => SemanticTokenType::new("class"),
      HighlightKind::Object => SemanticTokenType::new("object"),
      HighlightKind::Function => SemanticTokenType::new("function"),
      HighlightKind::Keyword => SemanticTokenType::new("keyword"),
      HighlightKind::Number => SemanticTokenType::new("number"),
      HighlightKind::String => SemanticTokenType::new("string"),
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
) -> Result<Vec<lsp_types::SemanticToken>, Box<dyn Error>> {
  let line_index = snap.analysis.line_index(file)?;

  let mut tokens = Vec::new();

  let mut line = 0;
  let mut col = 0;

  info!("highlight: {:?}", highlight.tokens);

  for h in highlight.tokens.iter() {
    let range = h.range;

    let pos = line_index.line_col(range.start());

    let delta_line = pos.line - line;
    if delta_line != 0 {
      col = 0;
    }
    let delta_start = pos.col - col;

    line = pos.line;
    col = pos.col;

    tokens.push(lsp_types::SemanticToken {
      delta_line,
      delta_start,
      length: (range.end() - range.start()).into(),
      token_type: h.kind as u32,
      token_modifiers_bitset: 0,
    });
  }

  Ok(tokens)
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
