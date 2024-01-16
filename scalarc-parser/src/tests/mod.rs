use core::fmt;

use crate::{EntryPoint, Event, Lexer};

mod inline;

pub fn lex(text: &str) -> String { format_events(&lex_events(text)) }

pub fn lex_events(text: &str) -> Vec<Event> { EntryPoint::SourceFile.parse(&mut Lexer::new(text)) }

pub fn format_events(events: &[Event]) -> String { Events(events).to_string() }

struct Events<'a>(&'a [Event]);

impl fmt::Display for Events<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut indent = 0;
    for e in self.0 {
      match e {
        Event::Start { kind } => {
          writeln!(f, "{}{:?}", "  ".repeat(indent), kind)?;
          indent += 1;
        }
        Event::Finish => indent -= 1,
        Event::Token { kind } => {
          writeln!(f, "{}{:?}", "  ".repeat(indent), kind)?;
        }
        Event::Error { msg } => {
          writeln!(f, "{}error: {}", "  ".repeat(indent), msg)?;
        }
        _ => todo!("event {e:?}"),
      }
    }
    Ok(())
  }
}
