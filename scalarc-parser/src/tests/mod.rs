use core::fmt;

use crate::{EntryPoint, Event, Lexer, SyntaxKind};

mod inline;

pub fn check(text: &str, expected_tree: &str) {
  let actual_tree = lex(&format!("{}\n", text));
  let expected_tree = expected_tree
    .lines()
    .map(|l| l.strip_prefix("        ").unwrap_or(l))
    .collect::<Vec<_>>()
    .join("\n");
  if actual_tree.trim() != expected_tree.trim() {
    pretty_assertions::assert_eq!(expected_tree.trim(), actual_tree.trim());
  }
}

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
          // Ignore tombstones
          if kind == &SyntaxKind::TOMBSTONE {
            continue;
          }
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
