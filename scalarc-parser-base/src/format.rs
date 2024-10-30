//! Debug formatting helpers for events.

use crate::Event;
use std::fmt::{self, Debug};

pub fn format_events<K: Debug>(events: &[Event<K>], text: &str) -> String {
  Events(events, text).to_string()
}
pub fn print_events<K: Debug>(events: &[Event<K>], text: &str) {
  println!("{}", Events(events, text))
}

struct Events<'a, K>(&'a [Event<K>], &'a str);

impl<K: Debug> fmt::Display for Events<'_, K> {
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
          writeln!(f, " '{}'", str.replace('\n', "\\n"))?;
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
