//! This module greps parser's code for specially formatted comments and turns
//! them into tests.

use super::lex;
use std::{
  fs,
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
    assert_eq!(events, test.expect);
  }
}

#[derive(Debug)]
struct Test {
  text:   String,
  expect: String,
}

fn collect_tests(s: &str) -> Vec<Test> {
  let mut tests = vec![];
  let mut in_test = false;
  let mut test_lines = vec![];
  for line in s.lines() {
    let line = line.trim();

    if in_test {
      if let Some(l) = line.strip_prefix("// ") {
        test_lines.push(l);
      } else {
        let src = test_lines.join("\n");
        let mut split = src.splitn(3, "---");
        let _ = split.next().unwrap();
        let text = split.next().unwrap().trim().into();
        let expect = split.next().unwrap().trim().into();

        tests.push(Test { text, expect });
        in_test = false;
        test_lines.clear();
      }
      continue;
    }

    if !line.starts_with("// test") {
      continue;
    }

    in_test = true;
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
