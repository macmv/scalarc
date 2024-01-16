use rowan::TextRange;

#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxError {
  message: String,
  range:   TextRange,
}

impl SyntaxError {
  pub fn new(message: impl Into<String>, range: TextRange) -> Self {
    SyntaxError { message: message.into(), range }
  }
}
