use std::ops::Range;

use scalarc_syntax::SyntaxError;

pub struct Diagnostic {
  pub message: String,
  pub span:    Range<u32>,
}

impl Diagnostic {
  pub fn from_syntax_error(error: &SyntaxError) -> Diagnostic {
    Diagnostic { message: error.message().into(), span: error.span() }
  }
}
