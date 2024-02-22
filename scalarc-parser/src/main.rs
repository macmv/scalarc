use scalarc_parser::{format::print_events, EntryPoint, Lexer};

fn main() {
  let name = match std::env::args().nth(1) {
    Some(it) => it,
    None => {
      eprintln!("Usage: scalarc-parser <file>");
      std::process::exit(1);
    }
  };

  let src = std::fs::read_to_string(&name).unwrap();

  let mut events = EntryPoint::SourceFile.parse(&mut Lexer::new(&src));
  let processed = scalarc_parser::process_events(&mut events);
  print_events(&processed, &src);

  if processed.iter().any(|e| matches!(e, scalarc_parser::Event::Error { .. })) {
    std::process::exit(1);
  }
}
