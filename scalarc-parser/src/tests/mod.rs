use core::fmt;

use crate::{EntryPoint, Event, Lexer, SyntaxKind};

mod inline;

pub fn check_expr(text: &str, expected_tree: &str) {
  check_inner(EntryPoint::Expr, text, expected_tree);
}
pub fn check(text: &str, expected_tree: &str) {
  check_inner(EntryPoint::SourceFile, text, expected_tree);
}

fn check_inner(entry_point: EntryPoint, text: &str, expected_tree: &str) {
  let actual_tree = lex(entry_point, &format!("{}\n", text));
  let expected_tree = expected_tree
    .lines()
    .map(|l| l.strip_prefix("        ").unwrap_or(l))
    .collect::<Vec<_>>()
    .join("\n");
  if actual_tree.trim() != expected_tree.trim() {
    pretty_assertions::assert_eq!(expected_tree.trim(), actual_tree.trim());
  }
}

pub fn lex(entry_point: EntryPoint, text: &str) -> String {
  format_events(&lex_events(entry_point, text), text)
}

pub fn lex_events(entry_point: EntryPoint, text: &str) -> Vec<Event> {
  entry_point.parse(&mut Lexer::new(text))
}

pub fn format_events(events: &[Event], text: &str) -> String { Events(events, text).to_string() }

struct Events<'a>(&'a [Event], &'a str);

impl fmt::Display for Events<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut indent = 0;
    let mut index = 0;
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
          writeln!(f, "{}{:?} '{}'", "  ".repeat(indent), kind, &self.1[index..index + 1])?;
          index += 1;
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
