use scalarc_test::Expect;

use crate::{format::format_events, EntryPoint, Event, Lexer};

mod inline;
mod syntax;

pub fn check_expr(text: &str, expected_tree: Expect) {
  check_inner(EntryPoint::Expr, text, expected_tree);
}
pub fn check(text: &str, expected_tree: Expect) {
  check_inner(EntryPoint::SourceFile, text, expected_tree);
}

fn check_inner(entry_point: EntryPoint, text: &str, expected_tree: Expect) {
  let actual_tree = lex(entry_point, text);

  expected_tree.assert_eq(&actual_tree);
}

pub fn lex(entry_point: EntryPoint, text: &str) -> String {
  format_events(&lex_events(entry_point, text), text)
}

pub fn lex_events(entry_point: EntryPoint, text: &str) -> Vec<Event> {
  let mut events = entry_point.parse(&mut Lexer::new(text));

  crate::process_events(&mut events)
}
