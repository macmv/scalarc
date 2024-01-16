use crate::{EntryPoint, Event, Lexer};

mod inline;

pub fn lex(text: &str) -> String { format_events(&lex_events(text)) }

pub fn lex_events(text: &str) -> Vec<Event> { EntryPoint::SourceFile.parse(&mut Lexer::new(text)) }

pub fn format_events(events: &[Event]) -> String {
  let mut buf = String::new();
  let mut indent = 0;
  for e in events {
    match e {
      Event::Start { kind } => {
        buf.push_str(&format!("{}{:?}\n", "  ".repeat(indent), kind));
        indent += 1;
      }
      Event::Finish => indent -= 1,
      Event::Token { kind } => buf.push_str(&format!("{}{:?}\n", "  ".repeat(indent), kind)),
      Event::Error { msg } => buf.push_str(&format!("{}error: {}\n", "  ".repeat(indent), msg)),
      _ => todo!("event {e:?}"),
    }
  }
  buf.trim().into()
}
