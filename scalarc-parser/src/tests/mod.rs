use core::fmt;

use scalarc_test::Expect;

use crate::{EntryPoint, Event, Lexer, SyntaxKind};
use std::mem;

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
  let mut events = entry_point.parse(&mut Lexer::new(text));
  let processed = process_events(&mut events);
  processed
}

pub fn format_events(events: &[Event], text: &str) -> String { Events(events, text).to_string() }

// Removes all the forward_parents. TODO: Produce a new Output type or something
// to avoid exposing Event.
fn process_events(events: &mut [Event]) -> Vec<Event> {
  let mut out = vec![];
  let mut forward_parents = Vec::new();

  for i in 0..events.len() {
    match mem::replace(&mut events[i], Event::tombstone()) {
      Event::Start { kind, forward_parent } => {
        // For events[A, B, C], B is A's forward_parent, C is B's forward_parent,
        // in the normal control flow, the parent-child relation: `A -> B -> C`,
        // while with the magic forward_parent, it writes: `C <- B <- A`.

        // append `A` into parents.
        forward_parents.push(kind);
        let mut idx = i;
        let mut fp = forward_parent;
        while let Some(fwd) = fp {
          idx += fwd as usize;
          // append `A`'s forward_parent `B`
          fp = match mem::replace(&mut events[idx], Event::tombstone()) {
            Event::Start { kind, forward_parent } => {
              forward_parents.push(kind);
              forward_parent
            }
            _ => unreachable!(),
          };
          // append `B`'s forward_parent `C` in the next stage.
        }

        for kind in forward_parents.drain(..).rev() {
          if kind != SyntaxKind::TOMBSTONE {
            out.push(Event::Start { kind, forward_parent: None });
          }
        }
      }
      Event::Finish => {
        out.push(Event::Finish);
      }
      Event::Token { kind, len } => {
        if kind != SyntaxKind::TOMBSTONE {
          out.push(Event::Token { kind, len });
        }
      }
      Event::Error { msg } => out.push(Event::Error { msg }),
    }
  }

  out
}

struct Events<'a>(&'a [Event], &'a str);

impl fmt::Display for Events<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut indent = 0;
    let mut index = 0;
    for e in self.0 {
      match e {
        Event::Start { kind, .. } => {
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
      }
    }
    Ok(())
  }
}
