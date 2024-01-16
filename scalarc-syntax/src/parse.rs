//! Some high level functions to wrap `scalarc_parser`.

use crate::{node::Scala, SyntaxError};
use rowan::{GreenNode, GreenNodeBuilder, Language, TextRange, TextSize};
use scalarc_parser::{Event, SyntaxKind};

pub fn parse_text(text: &str) -> (GreenNode, Vec<SyntaxError>) {
  let mut lexer = scalarc_parser::Lexer::new(text);

  let events = scalarc_parser::EntryPoint::SourceFile.parse(&mut lexer);
  build_tree(events)
}

fn build_tree(events: Vec<scalarc_parser::Event>) -> (GreenNode, Vec<SyntaxError>) {
  let mut builder = SyntaxTreeBuilder::new();

  for event in events {
    // TODO: hrm
    /*
    match event {
      Event::Token { kind } => {
        builder.token(kind, text);
      }
      Event::Start { kind, .. } => builder.start_node(kind),
      Event::Finish => builder.finish_node(),
      Event::Error { msg, pos } => builder.error(msg.to_string(), pos.try_into().unwrap()),
    }
    */
  }

  let (node, mut errors) = builder.finish_raw();
  // TODO: Collect lexer errors
  /*
  for (i, err) in lexer.errors() {
    let text_range = lexer.text_range(i);
    let text_range =
      TextRange::new(text_range.start.try_into().unwrap(), text_range.end.try_into().unwrap());
    errors.push(SyntaxError::new(err, text_range))
  }
  */

  (node, errors)
}

struct SyntaxTreeBuilder {
  errors:  Vec<SyntaxError>,
  builder: GreenNodeBuilder<'static>,
}
impl SyntaxTreeBuilder {
  pub fn new() -> Self { SyntaxTreeBuilder { errors: vec![], builder: GreenNodeBuilder::new() } }

  pub fn finish_raw(self) -> (GreenNode, Vec<SyntaxError>) {
    let green = self.builder.finish();
    (green, self.errors)
  }

  pub fn token(&mut self, kind: SyntaxKind, text: &str) {
    let kind = Scala::kind_to_raw(kind);
    self.builder.token(kind, text);
  }

  pub fn start_node(&mut self, kind: SyntaxKind) {
    let kind = Scala::kind_to_raw(kind);
    self.builder.start_node(kind);
  }

  pub fn finish_node(&mut self) { self.builder.finish_node(); }

  pub fn error(&mut self, error: String, text_pos: TextSize) {
    self.errors.push(SyntaxError::new_at_offset(error, text_pos));
  }
}
