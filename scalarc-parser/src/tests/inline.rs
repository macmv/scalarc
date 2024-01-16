//! This module greps parser's code for specially formatted comments and turns
//! them into tests.

use super::lex;
use std::{
  fs, iter,
  path::{Path, PathBuf},
};

pub fn project_root() -> PathBuf {
  let dir = env!("CARGO_MANIFEST_DIR");
  let res = PathBuf::from(dir).parent().unwrap().to_owned();
  assert!(res.join("rustfmt.toml").exists());
  res
}

#[test]
fn grammar_inline_tests() {
  let grammar_dir = project_root().join(Path::new("scalarc-parser/src/grammar"));
  let tests = tests_from_dir(&grammar_dir);

  for test in tests {
    let events = lex(&test.text);
    dbg!(&events);
  }
  panic!();
}

#[derive(Debug)]
struct Test {
  text: String,
  ok:   bool,
}

fn collect_tests(s: &str) -> Vec<Test> {
  let mut tests = vec![];
  let mut in_test = None;
  let mut test_lines = vec![];
  for line in s.lines() {
    let line = line.trim();

    if let Some(ok) = in_test {
      if let Some(l) = line.strip_prefix("// ") {
        test_lines.push(l);
      } else {
        tests.push(Test { text: test_lines.join("\n"), ok });
        in_test = None;
        test_lines.clear();
      }
      continue;
    }

    if !line.starts_with("// test") {
      continue;
    }

    let kind = line.split(' ').nth(2).unwrap();
    match kind {
      "ok" => in_test = Some(true),
      "err" => in_test = Some(false),
      _ => panic!("invalid test kind {kind}"),
    }
  }

  tests
}

fn tests_from_dir(dir: &Path) -> Vec<Test> {
  let mut tests = vec![];
  for entry in dir.read_dir().unwrap() {
    let entry = entry.unwrap();
    let path = entry.path();
    if path.extension().unwrap_or_default() != "rs" {
      continue;
    }

    process_file(&mut tests, &path);
  }

  fn process_file(tests: &mut Vec<Test>, path: &Path) {
    let text = fs::read_to_string(path).unwrap();

    for test in collect_tests(&text) {
      tests.push(test);
    }
  }

  tests
}
