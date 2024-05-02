//! Minimalistic snapshot testing for Rust. This is a fork of
//! https://github.com/rust-analyzer/expect-test.
//!
//! The main changes I've made are:
//! - Update the syntax to `expect![@""]` insteadd of `expect![[""]]`.
//! - Change indents to 2 spaces instead of 4.
//! - Remove expect_file![].
//!
//! # Introduction
//!
//! `scalarc_test` is a small addition over plain `assert_eq!` testing approach,
//! which allows to automatically update tests results.
//!
//! The core of the library is the `expect!` macro. It can be though of as a
//! super-charged string literal, which can update itself.
//!
//! Let's see an example:
//!
//! ```no_run
//! use scalarc_test::expect;
//!
//! let actual = 2 + 2;
//! let expected = expect![@"5"];
//! expected.assert_eq(&actual.to_string())
//! ```
//!
//! Running this code will produce a test failure, as `"5"` is indeed not equal
//! to `"4"`. Running the test with `BLESS=1` env variable however would
//! "magically" update the code to:
//!
//! ```no_run
//! # use scalarc_test::expect;
//! let actual = 2 + 2;
//! let expected = expect![@"4"];
//! expected.assert_eq(&actual.to_string())
//! ```
//!
//! This becomes very useful when you have a lot of tests with verbose and
//! potentially changing expected output.
//!
//! Under the hood, the `expect!` macro uses `file!`, `line!` and `column!` to
//! record source position at compile time. At runtime, this position is used
//! to patch the file in-place, if `BLESS` is set.
//!
//! # Guide
//!
//! `expect!` returns an instance of `Expect` struct, which holds position
//! information and a string literal. Use `Expect::assert_eq` for string
//! comparison. Use `Expect::assert_debug_eq` for verbose debug comparison. Note
//! that leading indentation is automatically removed.
//!
//! ```
//! use scalarc_test::expect;
//!
//! #[derive(Debug)]
//! struct Foo {
//!     value: i32,
//! }
//!
//! let actual = Foo { value: 92 };
//! let expected = expect![@"
//!     Foo {
//!         value: 92,
//!     }
//! "];
//! expected.assert_debug_eq(&actual);
//! ```
//!
//! Be careful with `assert_debug_eq` - in general, stability of the debug
//! representation is not guaranteed. However, even if it changes, you can
//! quickly update all the tests by running the test suite with `BLESS`
//! environmental variable set.
//!
//! If the expected data is too verbose to include inline, you can store it in
//! an external file using the `expect_file!` macro:
//!
//! ```no_run,ignore
//! use scalarc_test::expect_file;
//!
//! let actual = 42;
//! let expected = expect_file!["./the-answer.txt"];
//! expected.assert_eq(&actual.to_string());
//! ```
//!
//! File path is relative to the current file.
//!
//! # Suggested Workflows
//!
//! I like to use data-driven tests with `scalarc_test`. I usually define a
//! single driver function `check` and then call it from individual tests:
//!
//! ```
//! use scalarc_test::{expect, Expect};
//!
//! fn check(actual: i32, expect: Expect) {
//!   let actual = actual.to_string();
//!   expect.assert_eq(&actual);
//! }
//!
//! #[test]
//! fn test_addition() {
//!   check(90 + 2, expect![["92"]]);
//! }
//!
//! #[test]
//! fn test_multiplication() {
//!   check(46 * 2, expect![["92"]]);
//! }
//! ```
//!
//! Each test's body is a single call to `check`. All the variation in tests
//! comes from the input data.
//!
//! When writing a new test, I usually copy-paste an old one, leave the `expect`
//! blank and use `BLESS` to fill the value for me:
//!
//! ```
//! # use scalarc_test::{expect, Expect};
//! # fn check(_: i32, _: Expect) {}
//! #[test]
//! fn test_division() {
//!     check(92 / 2, expect![[]])
//! }
//! ```
//!
//! See
//! https://blog.janestreet.com/using-ascii-waveforms-to-test-hardware-designs/
//! for a cool example of snapshot testing in the wild!
//!
//! # Alternatives
//!
//! * [insta](https://crates.io/crates/insta) - a more feature full snapshot
//!   testing library.
//! * [k9](https://crates.io/crates/k9) - a testing library which includes
//!   support for snapshot testing among other things.
//!
//! # Maintenance status
//!
//! The main customer of this library is rust-analyzer. The library is  stable,
//! it is planned to not release any major versions past 1.0.
//!
//! ## Minimal Supported Rust Version
//!
//! This crate's minimum supported `rustc` version is `1.45.0`. MSRV is updated
//! conservatively, supporting roughly 10 minor versions of `rustc`. MSRV bump
//! is not considered semver breaking, but will require at least minor version
//! bump.
use parking_lot::Mutex;
use std::{
  collections::HashMap,
  convert::TryInto,
  env, fmt, fs, mem,
  ops::Range,
  panic,
  path::{Path, PathBuf},
};

const HELP: &str = "
You can update all `expect!` tests by running:

    BLESS=1 cargo test

To update a single test, place the cursor on `expect` token and use `run` feature of rust-analyzer.
";

fn update_expect() -> bool { env::var("BLESS").is_ok() }

/// Creates an instance of `Expect` from string literal:
///
/// ```
/// # use scalarc_test::expect;
/// expect![@"
///     Foo { value: 92 }
/// "];
/// expect![@r#"{"Foo": 92}"#];
/// ```
///
/// Leading indentation is stripped.
#[macro_export]
macro_rules! expect {
  [@$data:literal] => { $crate::Expect
    {
      position: $crate::Position {
        file: file!(),
        line: line!(),
        column: column!(),
      },
      data: $data,
      indent: true,
    }
  };
}

/// Self-updating string literal.
#[derive(Debug)]
pub struct Expect {
  #[doc(hidden)]
  pub position: Position,
  #[doc(hidden)]
  pub data:     &'static str,
  #[doc(hidden)]
  pub indent:   bool,
}

/// Position of original `expect!` in the source file.
#[derive(Debug)]
pub struct Position {
  #[doc(hidden)]
  pub file:   &'static str,
  #[doc(hidden)]
  pub line:   u32,
  #[doc(hidden)]
  pub column: u32,
}

impl fmt::Display for Position {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}:{}", self.file, self.line, self.column)
  }
}

#[derive(Clone, Copy)]
enum StrLitKind {
  Normal,
  Raw(usize),
}

impl StrLitKind {
  fn write_start(self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
    match self {
      Self::Normal => write!(w, "\""),
      Self::Raw(n) => {
        write!(w, "r")?;
        for _ in 0..n {
          write!(w, "#")?;
        }
        write!(w, "\"")
      }
    }
  }

  fn write_end(self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
    match self {
      Self::Normal => write!(w, "\""),
      Self::Raw(n) => {
        write!(w, "\"")?;
        for _ in 0..n {
          write!(w, "#")?;
        }
        Ok(())
      }
    }
  }
}

impl Expect {
  /// Checks if this expect is equal to `actual`.
  pub fn assert_eq(&self, actual: &str) {
    let trimmed = self.trimmed();
    if trimmed == actual {
      return;
    }
    Runtime::fail_expect(self, &trimmed, actual);
  }
  /// Checks if this expect is equal to `format!("{:#?}", actual)`.
  ///
  /// TODO: This will be removed in the future.
  pub fn assert_debug_eq(&self, actual: &impl fmt::Debug) {
    let actual = format!("{:#?}\n", actual);
    self.assert_eq(&actual)
  }
  /// If `true` (default), in-place update will indent the string literal.
  pub fn indent(&mut self, yes: bool) { self.indent = yes; }

  /// Returns the content of this expect.
  pub fn data(&self) -> &str { self.data }

  fn trimmed(&self) -> String {
    if !self.data.contains('\n') {
      return self.data.to_string();
    }
    trim_indent(self.data)
  }

  fn locate(&self, file: &str) -> Location {
    let mut target_line = None;
    let mut line_start = 0;
    for (i, line) in lines_with_ends(file).enumerate() {
      if i == self.position.line as usize - 1 {
        // `column` points to the first character of the macro invocation:
        //
        //    expect![[r#""#]]        expect![""]
        //    ^       ^               ^       ^
        //  column   offset                 offset
        //
        // Seek past the exclam, then skip any whitespace and
        // the macro delimiter to get to our argument.
        let byte_offset = line
          .char_indices()
          .skip((self.position.column - 1).try_into().unwrap())
          .skip_while(|&(_, c)| c != '!')
          .skip(1) // !
          .skip_while(|&(_, c)| c.is_whitespace())
          .skip(1) // [({
          .skip_while(|&(_, c)| c.is_whitespace())
          .next()
          .expect("Failed to parse macro invocation")
          .0;

        let literal_start = line_start + byte_offset;
        let indent = line.chars().take_while(|&it| it == ' ').count();
        target_line = Some((literal_start, indent));
        break;
      }
      line_start += line.len();
    }
    let (literal_start, line_indent) = target_line.unwrap();

    let lit_to_eof = &file[literal_start..];
    let lit_to_eof_trimmed = lit_to_eof.trim_start();

    let literal_start = literal_start + (lit_to_eof.len() - lit_to_eof_trimmed.len());

    let literal_len =
      locate_end(lit_to_eof_trimmed).expect("Couldn't find closing delimiter for `expect!`.");
    let literal_range = literal_start..literal_start + literal_len;
    Location { line_indent, literal_range }
  }
}

#[track_caller]
fn locate_end(arg_start_to_eof: &str) -> Option<usize> {
  // check for the `@` in
  // expect![@""];
  if arg_start_to_eof.chars().next()? != '@' {
    return None;
  }

  match arg_start_to_eof.chars().nth(1) {
    Some(c) if c.is_whitespace() => panic!("skip whitespace before calling `locate_end`"),

    // expect![] | expect!{} | expect!()
    None => Some(0),

    // expect![@"..."] | expect![r#"..."#]
    Some(_) => find_str_lit_len(&arg_start_to_eof[1..]).map(|p| p + 1),
  }
}

/// Parses a string literal, returning the byte index of its last character
/// (either a quote or a hash).
fn find_str_lit_len(str_lit_to_eof: &str) -> Option<usize> {
  use StrLitKind::*;

  fn try_find_n_hashes(
    s: &mut impl Iterator<Item = char>,
    desired_hashes: usize,
  ) -> Option<(usize, Option<char>)> {
    let mut n = 0;
    loop {
      match s.next()? {
        '#' => n += 1,
        c => return Some((n, Some(c))),
      }

      if n == desired_hashes {
        return Some((n, None));
      }
    }
  }

  let mut s = str_lit_to_eof.chars();
  let kind = match s.next()? {
    '"' => Normal,
    'r' => {
      let (n, c) = try_find_n_hashes(&mut s, usize::MAX)?;
      if c != Some('"') {
        return None;
      }
      Raw(n)
    }
    _ => return None,
  };

  let mut oldc = None;
  loop {
    let c = oldc.take().or_else(|| s.next())?;
    match (c, kind) {
      ('\\', Normal) => {
        let _escaped = s.next()?;
      }
      ('"', Normal) => break,
      ('"', Raw(0)) => break,
      ('"', Raw(n)) => {
        let (seen, c) = try_find_n_hashes(&mut s, n)?;
        if seen == n {
          break;
        }
        oldc = c;
      }
      _ => {}
    }
  }

  Some(str_lit_to_eof.len() - s.as_str().len())
}

#[derive(Default)]
struct Runtime {
  help_printed: bool,
  per_file:     HashMap<&'static str, FileRuntime>,
}
static RT: Mutex<Option<Runtime>> = Mutex::new(None);

impl Runtime {
  fn fail_expect(expect: &Expect, expected: &str, actual: &str) {
    let mut rt = RT.lock();
    let rt = rt.get_or_insert_with(Default::default);
    if update_expect() {
      println!("\x1b[1m\x1b[92mupdating\x1b[0m: {}", expect.position);
      rt.per_file
        .entry(expect.position.file)
        .or_insert_with(|| FileRuntime::new(expect))
        .update(expect, actual);
      return;
    }
    rt.panic(expect.position.to_string(), expected, actual);
  }

  fn panic(&mut self, position: String, expected: &str, actual: &str) {
    let print_help = !mem::replace(&mut self.help_printed, true);
    let help = if print_help { HELP } else { "" };

    let diff = pretty_assertions::StrComparison::new(expected, actual);

    println!(
      "\n
\x1b[1m\x1b[91merror\x1b[97m: expect test failed\x1b[0m
   \x1b[1m\x1b[34m-->\x1b[0m {}
{}
\x1b[1mExpect\x1b[0m:
----
{}
----

\x1b[1mActual\x1b[0m:
----
{}
----

\x1b[1mDiff\x1b[0m:
----
{}
----
",
      position, help, expected, actual, diff
    );
    // Use resume_unwind instead of panic!() to prevent a backtrace, which is
    // unnecessary noise.
    panic::resume_unwind(Box::new(()));
  }
}

struct FileRuntime {
  path:          PathBuf,
  original_text: String,
  patchwork:     Patchwork,
}

impl FileRuntime {
  fn new(expect: &Expect) -> FileRuntime {
    let path = to_abs_ws_path(Path::new(expect.position.file));
    let original_text = fs::read_to_string(&path).unwrap();
    let patchwork = Patchwork::new(original_text.clone());
    FileRuntime { path, original_text, patchwork }
  }
  fn update(&mut self, expect: &Expect, actual: &str) {
    let loc = expect.locate(&self.original_text);
    let desired_indent = if expect.indent { Some(loc.line_indent) } else { None };
    let patch = format_patch(desired_indent, actual);
    self.patchwork.patch(loc.literal_range, &patch);
    fs::write(&self.path, &self.patchwork.text).unwrap()
  }
}

#[derive(Debug)]
struct Location {
  line_indent: usize,

  /// The byte range of the argument to `expect!`, including the inner `[]` if
  /// it exists.
  literal_range: Range<usize>,
}

#[derive(Debug)]
struct Patchwork {
  text:   String,
  indels: Vec<(Range<usize>, usize)>,
}

impl Patchwork {
  fn new(text: String) -> Patchwork { Patchwork { text, indels: Vec::new() } }
  fn patch(&mut self, mut range: Range<usize>, patch: &str) {
    self.indels.push((range.clone(), patch.len()));
    self.indels.sort_by_key(|(delete, _insert)| delete.start);

    let (delete, insert) = self
      .indels
      .iter()
      .take_while(|(delete, _)| delete.start < range.start)
      .map(|(delete, insert)| (delete.end - delete.start, insert))
      .fold((0usize, 0usize), |(x1, y1), (x2, y2)| (x1 + x2, y1 + y2));

    for pos in &mut [&mut range.start, &mut range.end] {
      **pos -= delete;
      **pos += insert;
    }

    self.text.replace_range(range, &patch);
  }
}

fn lit_kind_for_patch(patch: &str) -> StrLitKind {
  let has_dquote = patch.chars().any(|c| c == '"');
  if !has_dquote {
    let has_bslash_or_newline = patch.chars().any(|c| matches!(c, '\\' | '\n'));
    return if has_bslash_or_newline { StrLitKind::Raw(1) } else { StrLitKind::Normal };
  }

  // Find the maximum number of hashes that follow a double quote in the string.
  // We need to use one more than that to delimit the string.
  let leading_hashes = |s: &str| s.chars().take_while(|&c| c == '#').count();
  let max_hashes = patch.split('"').map(leading_hashes).max().unwrap();
  StrLitKind::Raw(max_hashes + 1)
}

fn format_patch(desired_indent: Option<usize>, patch: &str) -> String {
  let lit_kind = lit_kind_for_patch(patch);
  let indent = desired_indent.map(|it| " ".repeat(it));
  let is_multiline = patch.contains('\n');

  let mut buf = String::new();
  buf.push('@');
  lit_kind.write_start(&mut buf).unwrap();
  if is_multiline {
    buf.push('\n');
  }
  let mut final_newline = false;
  for line in lines_with_ends(patch) {
    if is_multiline && !line.trim().is_empty() {
      if let Some(indent) = &indent {
        buf.push_str(indent);
        buf.push_str("  ");
      }
    }
    buf.push_str(line);
    final_newline = line.ends_with('\n');
  }
  if final_newline {
    if let Some(indent) = &indent {
      buf.push_str(indent);
    }
  }
  lit_kind.write_end(&mut buf).unwrap();
  buf
}

fn workspace_root() -> PathBuf {
  let my_manifest = env::var("CARGO_MANIFEST_DIR").unwrap();
  let workspace_root = Path::new(&my_manifest)
    .ancestors()
    .filter(|it| it.join("Cargo.toml").exists())
    .last()
    .unwrap()
    .to_path_buf();

  workspace_root
}

fn to_abs_ws_path(path: &Path) -> PathBuf {
  if path.is_absolute() {
    return path.to_owned();
  }

  workspace_root().join(path)
}

fn trim_indent(mut text: &str) -> String {
  if text.starts_with('\n') {
    text = &text[1..];
  }
  let indent = text
    .lines()
    .filter(|it| !it.trim().is_empty())
    .map(|it| it.len() - it.trim_start().len())
    .min()
    .unwrap_or(0);

  lines_with_ends(text)
    .map(|line| if line.len() <= indent { line.trim_start_matches(' ') } else { &line[indent..] })
    .collect()
}

fn lines_with_ends(text: &str) -> LinesWithEnds { LinesWithEnds { text } }

struct LinesWithEnds<'a> {
  text: &'a str,
}

impl<'a> Iterator for LinesWithEnds<'a> {
  type Item = &'a str;
  fn next(&mut self) -> Option<&'a str> {
    if self.text.is_empty() {
      return None;
    }
    let idx = self.text.find('\n').map_or(self.text.len(), |it| it + 1);
    let (res, next) = self.text.split_at(idx);
    self.text = next;
    Some(res)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_trivial_assert() { expect![@"5"].assert_eq("5"); }

  #[test]
  fn test_format_patch() {
    let patch = format_patch(None, "hello\nworld\n");
    expect![@r##"
      @r#"
      hello
      world
      "#"##]
    .assert_eq(&patch);

    let patch = format_patch(None, r"hello\tworld");
    expect![@r##"@r#"hello\tworld"#"##].assert_eq(&patch);

    let patch = format_patch(None, "{\"foo\": 42}");
    expect![@r##"@r#"{"foo": 42}"#"##].assert_eq(&patch);

    let patch = format_patch(Some(0), "hello\nworld\n");
    expect![@r##"
      @r#"
        hello
        world
      "#"##]
    .assert_eq(&patch);

    let patch = format_patch(Some(4), "single line");
    expect![@r#""single line""#].assert_eq(&patch);
  }

  #[test]
  fn test_patchwork() {
    let mut patchwork = Patchwork::new("one two three".to_string());
    patchwork.patch(4..7, "zwei");
    patchwork.patch(0..3, "один");
    patchwork.patch(8..13, "3");
    // TODO: Remove assert_debug_eq.
    expect![@r#"
      Patchwork {
          text: "один zwei 3",
          indels: [
              (
                  0..3,
                  8,
              ),
              (
                  4..7,
                  4,
              ),
              (
                  8..13,
                  1,
              ),
          ],
      }
    "#]
    .assert_debug_eq(&patchwork);
  }

  #[test]
  fn smoke_test_indent() {
    fn check_indented(input: &str, mut expect: Expect) {
      expect.indent(true);
      expect.assert_eq(input);
    }
    fn check_not_indented(input: &str, mut expect: Expect) {
      expect.indent(false);
      expect.assert_eq(input);
    }

    check_indented(
      "\
line1
  line2
",
      expect![@r#"
          line1
            line2
        "#],
    );

    check_not_indented(
      "\
line1
  line2
",
      expect![@r#"
line1
  line2
"#],
    );
  }

  #[test]
  fn test_locate() {
    assert_eq!(locate_end(r#"@"hi"#), None);
    assert_eq!(locate_end(r#""hi"];"#), None);
    assert_eq!(locate_end(r#"@"hi"];"#), Some(5));
  }

  #[test]
  fn test_find_str_lit_len() {
    macro_rules! check_str_lit_len {
            ($( $s:literal ),* $(,)?) => {$({
                let lit = stringify!($s);
                assert_eq!(find_str_lit_len(lit), Some(lit.len()));
            })*}
        }

    check_str_lit_len![
      r##"foa\""#"##,
      r##"

                asdf][]]""""#
            "##,
      "",
      "\"",
      "\"\"",
      "#\"#\"#",
    ];
  }
}
