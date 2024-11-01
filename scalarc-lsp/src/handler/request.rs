use std::{error::Error, path::Path, sync::Arc};

use line_index::LineIndex;
use lsp_types::{SemanticTokenModifier, SemanticTokenType};
use scalarc_analysis::{
  completion::CompletionKind,
  highlight::{Highlight, HighlightKind},
};
use scalarc_hir::{FileLocation, GlobalDefinitionKind, HirDefinitionKind};
use scalarc_source::FileId;
use scalarc_syntax::{TextRange, TextSize};

use crate::global::GlobalStateSnapshot;

/// Converts file positions to LSP positions.
struct LspConverter {
  line_index: Arc<LineIndex>,
}

impl LspConverter {
  pub fn from_pos(
    snap: &GlobalStateSnapshot,
    pos: lsp_types::TextDocumentPositionParams,
  ) -> Result<(FileLocation, Self), Box<dyn Error>> {
    let pos = file_position(&snap, pos)?;

    Ok((pos, LspConverter::new(snap, pos.file)?))
  }

  pub fn new(snap: &GlobalStateSnapshot, file: FileId) -> Result<Self, Box<dyn Error>> {
    Ok(Self { line_index: snap.analysis.line_index(file)? })
  }

  pub fn pos(&self, index: TextSize) -> lsp_types::Position {
    let pos = self.line_index.line_col(index);
    lsp_types::Position { line: pos.line, character: pos.col }
  }

  pub fn range(&self, range: TextRange) -> lsp_types::Range {
    let start = self.pos(range.start());
    let end = self.pos(range.end());

    lsp_types::Range { start, end }
  }
}

pub fn handle_completion(
  snap: GlobalStateSnapshot,
  params: lsp_types::CompletionParams,
) -> Result<Option<lsp_types::CompletionResponse>, Box<dyn Error>> {
  if let Some(_) = snap.absolute_path(&params.text_document_position.text_document.uri) {
    let completions =
      snap.analysis.completions(file_position(&snap, params.text_document_position)?)?;

    Ok(Some(lsp_types::CompletionResponse::Array(
      completions
        .into_iter()
        .map(|c| {
          let (kind, detail) = match c.kind {
            CompletionKind::Hir(HirDefinitionKind::Val(_)) => {
              (lsp_types::CompletionItemKind::VARIABLE, None)
            }
            CompletionKind::Hir(HirDefinitionKind::Var(_)) => {
              (lsp_types::CompletionItemKind::VARIABLE, None)
            }
            CompletionKind::Hir(HirDefinitionKind::Parameter(_)) => {
              (lsp_types::CompletionItemKind::VARIABLE, None)
            }
            CompletionKind::Hir(HirDefinitionKind::Pattern) => {
              (lsp_types::CompletionItemKind::VARIABLE, None)
            }
            CompletionKind::Hir(HirDefinitionKind::Def(sig)) => {
              (lsp_types::CompletionItemKind::FUNCTION, Some(sig.to_string()))
            }
            CompletionKind::Hir(HirDefinitionKind::Import) => {
              // TODO
              unreachable!()
            }
            CompletionKind::Hir(HirDefinitionKind::Object(_)) => {
              (lsp_types::CompletionItemKind::CLASS, None)
            }
            CompletionKind::Global(GlobalDefinitionKind::Class(_, _)) => {
              (lsp_types::CompletionItemKind::CLASS, None)
            }
            CompletionKind::Global(GlobalDefinitionKind::Trait(_)) => {
              (lsp_types::CompletionItemKind::INTERFACE, None)
            }
            CompletionKind::Global(GlobalDefinitionKind::Object(_)) => {
              (lsp_types::CompletionItemKind::CLASS, None)
            }
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
  if let Some(path) = snap.absolute_path(&params.text_document.uri) {
    let file_id = snap.files.read().get_absolute(&path).ok_or("file not found")?;
    let highlight = snap.analysis.highlight(file_id)?;

    let tokens = to_semantic_tokens(snap, file_id, &highlight)?;

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
  let (cursor_pos, _) = LspConverter::from_pos(&snap, params.text_document_position_params)?;
  let definition = snap.analysis.definition_for_name(cursor_pos)?;

  if let Some((_, def_pos)) = definition {
    let converter = LspConverter::new(&snap, def_pos.file)?;
    let files = snap.files.read();

    Ok(Some(lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location::new(
      lsp_types::Url::parse(&format!(
        "file://{}",
        files.id_to_absolute_path(def_pos.file).display()
      ))
      .unwrap(),
      converter.range(def_pos.range),
    ))))
  } else {
    Ok(None)
  }
}

pub fn handle_document_highlight(
  snap: GlobalStateSnapshot,
  params: lsp_types::DocumentHighlightParams,
) -> Result<Option<Vec<lsp_types::DocumentHighlight>>, Box<dyn Error>> {
  let (cursor_pos, converter) =
    LspConverter::from_pos(&snap, params.text_document_position_params)?;
  let definition = snap.analysis.definition_for_name(cursor_pos)?;
  let refs = snap.analysis.references_for_name(cursor_pos)?;

  if let Some((_, def_pos)) = definition {
    if cursor_pos.file != def_pos.file {
      return Ok(None);
    }

    let def_highlight = lsp_types::DocumentHighlight {
      range: converter.range(def_pos.range),
      kind:  Some(lsp_types::DocumentHighlightKind::WRITE),
    };

    let refs_highlight = refs.into_iter().map(|r| lsp_types::DocumentHighlight {
      range: converter.range(r.pos.range),
      kind:  Some(lsp_types::DocumentHighlightKind::READ),
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
  let (pos, converter) = LspConverter::from_pos(&snap, params.text_document_position_params)?;
  let def = snap.analysis.definition_for_name(pos)?;
  let ty = snap.analysis.type_at(pos)?;

  let range = def.map(|(_, pos)| converter.range(pos.range));

  Ok(Some(lsp_types::Hover {
    range,
    contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(match ty {
      Some(ty) => ty.to_string(),
      None => "unknown type".to_string(),
    })),
  }))
}

struct TokenModifier {
  stat: bool,
}

impl TokenModifier {
  pub fn all() -> Vec<SemanticTokenModifier> { vec![SemanticTokenModifier::new("static")] }

  pub fn from_kind(kind: HighlightKind) -> Self {
    Self { stat: matches!(kind, HighlightKind::Object) }
  }

  pub fn encode(&self) -> u32 {
    let mut bits = 0;

    if self.stat {
      bits |= 1;
    }

    bits
  }
}

pub fn semantic_tokens_legend() -> lsp_types::SemanticTokensLegend {
  fn token_type(kind: HighlightKind) -> SemanticTokenType {
    match kind {
      HighlightKind::Class => SemanticTokenType::new("class"),
      HighlightKind::Trait => SemanticTokenType::new("interface"),
      HighlightKind::Object => SemanticTokenType::new("class"),
      HighlightKind::Function => SemanticTokenType::new("function"),
      HighlightKind::Keyword => SemanticTokenType::new("keyword"),
      HighlightKind::Number => SemanticTokenType::new("number"),
      HighlightKind::String => SemanticTokenType::new("string"),
      HighlightKind::Parameter => SemanticTokenType::new("parameter"),
      HighlightKind::Type => SemanticTokenType::new("type"),
      HighlightKind::Variable => SemanticTokenType::new("variable"),
    }
  }

  lsp_types::SemanticTokensLegend {
    token_types:     HighlightKind::iter().map(token_type).collect(),
    token_modifiers: TokenModifier::all(),
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
      token_modifiers_bitset: TokenModifier::from_kind(h.kind).encode(),
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
  let file_id = files.get_absolute(path).ok_or("file not found")?;

  let index = snap.analysis.line_index(file_id)?;

  match index.offset(line_index::LineCol { line: pos.position.line, col: pos.position.character }) {
    Some(index) => Ok(FileLocation { file: file_id, index }),
    None => Err("position not found".into()),
  }
}
