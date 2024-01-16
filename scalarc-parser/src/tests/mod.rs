use core::fmt;

use scalarc_test::Expect;

use crate::{EntryPoint, Event, Lexer, SyntaxKind};

mod inline;

pub fn check_expr(text: &str, expected_tree: Expect) {
  check_inner(EntryPoint::Expr, text, expected_tree);
}
pub fn check(text: &str, expected_tree: Expect) {
  check_inner(EntryPoint::SourceFile, text, expected_tree);
}

fn check_inner(entry_point: EntryPoint, text: &str, expected_tree: Expect) {
  let actual_tree = lex(entry_point, &format!("{}\n", text));

  expected_tree.assert_eq(&actual_tree);
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
        Event::Token { kind, len } => {
          write!(f, "{}{:?}", "  ".repeat(indent), kind)?;
          if index >= self.1.len() {
            writeln!(f, " <EOF>")?;
            continue;
          }
          let str = &self.1[index..index + len];
          if str == "\n" {
            writeln!(f, " '\\n'")?;
          } else {
            writeln!(f, " '{str}'")?;
          }
          index += len;
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
