use core::fmt;

use crate::{token::Token, EntryPoint, Event, Lexer, SyntaxKind};

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
  let mut input = vec![];
  // TODO: Don't run the lexer twice!
  let mut lex = Lexer::new(text);
  while let Ok(t) = lex.next() {
    input.push(crate::token_to_kind(t, lex.slice()));
  }
  let tree = entry_point.parse(&mut Lexer::new(text));
  intersperse_trivia(input, tree)
}

fn intersperse_trivia(input: Vec<SyntaxKind>, tree: Vec<Event>) -> Vec<Event> {
  let mut builder = TriviaBuilder::new(input);
  for event in tree {
    match event {
      Event::Start { kind } => builder.start(kind),
      Event::Finish => builder.finish(),
      Event::Token { kind } => {
        builder.token(kind);
      }
      Event::Error { msg } => builder.error(msg),
      _ => todo!(),
    }
  }

  builder.output
}

struct TriviaBuilder {
  input:  Vec<SyntaxKind>,
  pos:    u32,
  output: Vec<Event>,
}

impl TriviaBuilder {
  fn new(input: Vec<SyntaxKind>) -> Self { TriviaBuilder { input, pos: 0, output: Vec::new() } }

  fn eat_trivia(&mut self) {
    loop {
      let t = self.input.get(self.pos as usize).copied().unwrap_or(SyntaxKind::EOF);
      if t == SyntaxKind::WHITESPACE || t == SyntaxKind::COMMENT {
        self.output.push(Event::Token { kind: SyntaxKind::WHITESPACE });
        self.pos += 1;
      } else {
        break;
      }
    }
  }

  fn start(&mut self, kind: SyntaxKind) {
    self.eat_trivia();
    self.output.push(Event::Start { kind });
  }

  fn finish(&mut self) { self.output.push(Event::Finish); }

  fn token(&mut self, kind: SyntaxKind) {
    self.eat_trivia();
    self.output.push(Event::Token { kind });
    self.pos += 1;
  }

  fn error(&mut self, msg: String) { self.output.push(Event::Error { msg }); }
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
          write!(f, "{}{:?}", "  ".repeat(indent), kind)?;
          if index >= self.1.len() {
            writeln!(f, " <EOF>")?;
            continue;
          }
          let str = &self.1[index..index + 1];
          if str == "\n" {
            writeln!(f, " '\\n'")?;
          } else {
            writeln!(f, " '{str}'")?;
          }
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
