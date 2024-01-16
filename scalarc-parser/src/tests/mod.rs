use crate::{EntryPoint, Event, Lexer, Parser, T};

mod inline;

pub fn lex(text: &str) -> Vec<Event> {
  let mut lexer = Lexer::new(text);
  EntryPoint::SourceFile.parse(&mut Lexer::new(text))
}
