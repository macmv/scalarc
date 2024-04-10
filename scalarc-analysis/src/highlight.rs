use scalarc_syntax::{node::SyntaxToken, Parse, SourceFile, TextRange};

#[derive(Debug, Clone)]
pub struct Highlight {
  pub tokens: Vec<HighlightToken>,
}

#[derive(Debug, Clone)]
pub struct HighlightToken {
  pub range:      TextRange,
  pub kind:       HighlightKind,
  pub modifierst: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum HighlightKind {
  Class,
  Function,
  Keyword,
  Parameter,
  // Keep last!
  Variable,
}

impl Highlight {
  pub fn new() -> Highlight { Highlight { tokens: Vec::new() } }

  pub fn from_ast(ast: Parse<SourceFile>) -> Highlight {
    let mut hl = Highlight::new();

    for item in ast.tree().items() {
      match item {
        scalarc_syntax::ast::Item::ClassDef(o) => {
          hl.highlight_opt(o.case_token(), HighlightKind::Keyword);
          hl.highlight_opt(o.class_token(), HighlightKind::Keyword);
          hl.highlight_opt(o.id_token(), HighlightKind::Class);
        }
        _ => {}
      }
    }

    hl
  }

  pub fn highlight_opt(&mut self, token: Option<SyntaxToken>, kind: HighlightKind) {
    if let Some(token) = token {
      self.highlight(token, kind);
    }
  }

  pub fn highlight(&mut self, token: SyntaxToken, kind: HighlightKind) -> &mut HighlightToken {
    self.tokens.push(HighlightToken { range: token.text_range(), kind, modifierst: 0 });
    self.tokens.last_mut().unwrap()
  }
}

impl HighlightKind {
  pub fn iter() -> impl Iterator<Item = HighlightKind> {
    (0..=HighlightKind::Variable as u8).map(|i| unsafe { std::mem::transmute(i) })
  }
}
