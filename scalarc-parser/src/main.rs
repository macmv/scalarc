use scalarc_parser::{format::print_events, EntryPoint, Event, Lexer};

fn main() {
  let files = std::env::args().skip(1).collect::<Vec<_>>();
  if files.is_empty() {
    eprintln!("Usage: scalarc-parser <file>...");
    std::process::exit(1);
  }

  let mut is_err = false;
  for file in files {
    if parse_file(&file).is_err() {
      is_err = true;
      break;
    }
  }
  if is_err {
    std::process::exit(1);
  }
}

fn parse_file(name: &str) -> Result<(), ()> {
  let src = std::fs::read_to_string(name).map_err(|e| {
    eprintln!("error: {}", e);
  })?;

  let mut events = EntryPoint::SourceFile.parse(&mut Lexer::new(&src));
  let processed = scalarc_parser::process_events(&mut events);
  print_events(&processed, &src);

  let mut line = 0;
  let mut col = 0;

  let mut index = 0;

  for e in &processed {
    match e {
      Event::Start { .. } => {}
      Event::Finish => {}
      Event::Token { len, .. } => {
        let str = &src[index..index + len];
        for c in str.chars() {
          if c == '\n' {
            line += 1;
            col = 0;
          } else {
            col += 1;
          }
        }
        index += len;
      }
      Event::Error { msg } => {
        println!("error: {msg}");

        let ln = line + 1;
        println!("at {ln}:{col}");

        let start = index - col;
        let mut end = index;
        if let Some(next) = src[end..].find('\n') {
          end += next;
        }

        let line_str = &src[start..end];

        let indent = (line as f64).log10().floor() as usize + 1;

        match format_args!("{: >1$}", " ", indent) {
          ii => {
            println!(" {ii} |");
            println!(" {ln} |{line_str}");
            println!(" {ii} |{: >1$}", "^", col + 1);
            println!(" {ii} |");
            println!();
          }
        }
      }
    }
  }

  if processed.iter().any(|e| matches!(e, scalarc_parser::Event::Error { .. })) {
    Err(())
  } else {
    Ok(())
  }
}
