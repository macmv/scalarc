use scalarc_syntax::{SyntaxError, TextRange};

pub struct Diagnostic {
  pub message: String,
  pub span:    TextRange,
}

impl Diagnostic {
  pub fn from_syntax_error(error: &SyntaxError) -> Diagnostic {
    Diagnostic { message: error.message().into(), span: error.span() }
  }
}
