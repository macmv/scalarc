use std::ops::Range;

use thiserror::Error;
use unicode_categories::UnicodeCategories;

// Scala's grammar is quite finicky. We expose a `Token` enum that is a
// parser-usable version of a token. It contains high level concepts like
// "Identifiers" and "Numbers".
//
// In order to actually parse identifiers (of all things), we have a separate
// inner token type, which implements the various parts of an identifier.

#[derive(Debug, PartialEq)]
pub enum Token {
  Ident(Ident),

  Literal(Literal),

  Group(Group),
  Delimiter(Delimiter),
  Newline,
  Whitespace,

  Invalid,
}

#[derive(Debug, PartialEq)]
pub enum Ident {
  Plain,
  Operator,
  Backtick,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
  Integer,
  Float,
  HexInteger,
  BinaryInteger,
}

pub type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug, Error, PartialEq)]
pub enum LexError {
  #[error("invalid character")]
  InvalidChar,

  #[error("string terminated in newline")]
  NewlineInString,

  #[error("missing closing char quote")]
  MissingCharClose,

  #[error("end of file reached")]
  EOF,
}

// Below we have the lexer internals.

#[derive(Clone, Copy, Debug, PartialEq)]
enum InnerToken {
  Whitespace,
  Newline,

  Underscore,
  Letter,
  Digit,
  Operator,

  Group(Group),
  Delimiter(Delimiter),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Group {
  OpenParen,
  CloseParen,

  OpenBracket,
  CloseBracket,

  OpenBrace,
  CloseBrace,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Delimiter {
  Backtick,
  SingleQuote,
  DoubleQuote,
  Dot,
  Semicolon,
  Comma,

  // Not quite tokens, but used to lex comments.
  Slash,
  Backslash,
  Star,
}

struct Tokenizer<'a> {
  source: &'a str,
  index:  usize,
}

impl<'a> Tokenizer<'a> {
  pub fn new(source: &'a str) -> Self { Tokenizer { source, index: 0 } }

  pub fn pos(&self) -> usize { self.index }

  pub fn peek(&mut self) -> Option<InnerToken> {
    if self.index >= self.source.len() {
      None
    } else {
      let chars = self.source[self.index..].chars().take(1);
      let t1 = self.eat();
      for c in chars {
        self.index -= c.len_utf8();
      }
      t1.ok()
    }
  }

  pub fn peek2(&mut self) -> Option<InnerToken> {
    if self.index >= self.source.len() {
      None
    } else {
      let chars = self.source[self.index..].chars().take(2);
      let _ = self.eat();
      let t2 = self.eat();
      for c in chars {
        self.index -= c.len_utf8();
      }
      t2.ok()
    }
  }

  pub fn peek_char(&self) -> Option<char> { self.source[self.index..].chars().next() }

  pub fn eat(&mut self) -> Result<InnerToken> {
    let Some(c) = self.source[self.index..].chars().next() else {
      return Err(LexError::EOF);
    };
    self.index += c.len_utf8();
    let t = match c {
      '\u{0020}' | '\u{0009}' | '\u{000d}' => InnerToken::Whitespace,
      '\n' => InnerToken::Newline,

      '(' => InnerToken::Group(Group::OpenParen),
      ')' => InnerToken::Group(Group::CloseParen),
      '[' => InnerToken::Group(Group::OpenBracket),
      ']' => InnerToken::Group(Group::CloseBracket),
      '{' => InnerToken::Group(Group::OpenBrace),
      '}' => InnerToken::Group(Group::CloseBrace),

      '`' => InnerToken::Delimiter(Delimiter::Backtick),
      '\'' => InnerToken::Delimiter(Delimiter::SingleQuote),
      '"' => InnerToken::Delimiter(Delimiter::DoubleQuote),
      '.' => InnerToken::Delimiter(Delimiter::Dot),
      ';' => InnerToken::Delimiter(Delimiter::Semicolon),
      ',' => InnerToken::Delimiter(Delimiter::Comma),
      '/' => InnerToken::Delimiter(Delimiter::Slash),
      '\\' => InnerToken::Delimiter(Delimiter::Backslash),
      '*' => InnerToken::Delimiter(Delimiter::Star),

      '_' => InnerToken::Underscore,
      'a'..='z' | 'A'..='Z' | '$' => InnerToken::Letter,
      '0'..='9' => InnerToken::Digit,
      '\u{0020}'..='\u{007e}' => InnerToken::Operator,

      // Lowercase letters.
      c if c.is_letter_lowercase() => InnerToken::Letter,
      // Uppercase letters.
      c if c.is_letter_uppercase() => InnerToken::Letter,
      c if c.is_letter_titlecase() => InnerToken::Letter,
      c if c.is_number_letter() => InnerToken::Letter,
      // Either lowercase or uppercase.
      c if c.is_letter_other() => InnerToken::Letter,
      c if c.is_letter_modifier() => InnerToken::Letter,

      // TODO: What to do with non-ascii characters?
      _ => InnerToken::Operator,
    };
    Ok(t)
  }

  pub fn span(&self) -> Range<usize> { self.index - 1..self.index }
}

pub struct Lexer<'a> {
  tok:  Tokenizer<'a>,
  span: Range<usize>,

  pub in_string: bool,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Lexer { tok: Tokenizer::new(input), span: 0..0, in_string: false }
  }

  fn ok(&mut self, start: usize, tok: Token) -> Result<Token> {
    self.span.start = start;
    self.span.end = self.tok.span().end;
    Ok(tok)
  }

  pub fn eat_whitespace(&mut self) -> Result<Option<Token>> {
    loop {
      match self.tok.peek() {
        Some(InnerToken::Whitespace) => {
          self.tok.eat().unwrap();
          return Ok(Some(Token::Whitespace));
        }
        Some(_) | None => break,
      }
    }
    Ok(None)
  }

  pub fn next(&mut self) -> Result<Token> {
    let start = self.tok.pos();
    if let Some(t) = self.eat_whitespace()? {
      while self.eat_whitespace()?.is_some() {}
      return self.ok(start, t);
    }

    let first = self.tok.eat()?;
    match first {
      // Line comments.
      InnerToken::Delimiter(Delimiter::Slash)
        if !self.in_string && self.tok.peek() == Some(first) =>
      {
        self.tok.eat()?;

        loop {
          match self.tok.peek() {
            Some(InnerToken::Newline) => break,
            None => break,
            _ => {
              self.tok.eat()?;
            }
          }
        }

        self.ok(start, Token::Whitespace)
      }

      // Block comments.
      InnerToken::Delimiter(Delimiter::Slash)
        if !self.in_string && self.tok.peek() == Some(InnerToken::Delimiter(Delimiter::Star)) =>
      {
        self.tok.eat()?;

        let mut depth = 1;

        loop {
          match self.tok.eat() {
            // `/*`
            Ok(InnerToken::Delimiter(Delimiter::Slash))
              if self.tok.peek() == Some(InnerToken::Delimiter(Delimiter::Star)) =>
            {
              depth += 1;
            }

            // `*/`
            Ok(InnerToken::Delimiter(Delimiter::Star))
              if self.tok.peek() == Some(InnerToken::Delimiter(Delimiter::Slash)) =>
            {
              depth -= 1;
            }

            Ok(_) => {}
            Err(LexError::EOF) => break,
            Err(e) => return Err(e),
          }

          if depth == 0 {
            self.tok.eat()?;
            break;
          }
        }

        self.ok(start, Token::Whitespace)
      }

      // Plain identifier.
      InnerToken::Underscore | InnerToken::Letter if !self.in_string => {
        // This is intentionally not `first == InnerToken::Underscore`, to match scala's
        // behavior. Honestly its probably a bug in scala. Ah well. It makes `_+` two
        // identifiers, but `__+` one identifier.
        let mut prev_was_underscore = false;
        let mut only_op = false;
        loop {
          let t = self.tok.peek();
          match t {
            Some(InnerToken::Letter | InnerToken::Digit) if !only_op => {}
            Some(InnerToken::Underscore) if !only_op => {}
            Some(InnerToken::Operator) if only_op || prev_was_underscore => only_op = true,
            Some(_) | None => break,
          }

          prev_was_underscore = t == Some(InnerToken::Underscore);

          self.tok.eat().unwrap();
        }

        self.ok(start, Token::Ident(Ident::Plain))
      }

      // Operator identifier.
      InnerToken::Operator
      | InnerToken::Delimiter(Delimiter::Slash)
      | InnerToken::Delimiter(Delimiter::Star)
      | InnerToken::Delimiter(Delimiter::Backslash)
        if !self.in_string =>
      {
        loop {
          match self.tok.peek() {
            Some(
              InnerToken::Operator
              | InnerToken::Delimiter(Delimiter::Slash)
              | InnerToken::Delimiter(Delimiter::Star)
              | InnerToken::Delimiter(Delimiter::Backslash),
            ) => {}
            Some(_) | None => break,
          }
          self.tok.eat().unwrap();
        }

        self.ok(start, Token::Ident(Ident::Operator))
      }

      // Backtick identifier.
      InnerToken::Delimiter(Delimiter::Backtick) if !self.in_string => {
        loop {
          match self.tok.eat() {
            Ok(InnerToken::Delimiter(Delimiter::Backtick)) | Err(LexError::EOF) => break,
            Ok(_) => {}
            Err(e) => return Err(e),
          }
        }

        self.ok(start, Token::Ident(Ident::Backtick))
      }

      // Numbers.
      InnerToken::Digit => match self.tok.peek_char() {
        Some('x' | 'X') => {
          self.tok.eat().unwrap();
          loop {
            match self.tok.peek_char() {
              Some('0'..='9' | 'a'..='f' | 'A'..='F' | '_') => {}
              Some(_) | None => break,
            }
            self.tok.eat().unwrap();
          }

          self.ok(start, Token::Literal(Literal::HexInteger))
        }

        Some('b' | 'B') => {
          self.tok.eat().unwrap();
          loop {
            match self.tok.peek_char() {
              Some('0'..='1' | '_') => {}
              Some(_) | None => break,
            }
            self.tok.eat().unwrap();
          }

          self.ok(start, Token::Literal(Literal::BinaryInteger))
        }

        _ => {
          let mut is_float = false;
          let mut is_exponent = false;
          loop {
            match self.tok.peek_char() {
              Some('0'..='9' | '_') => {}
              Some('.') => {
                if !is_float && self.tok.peek2() == Some(InnerToken::Digit) {
                  is_float = true;
                } else {
                  break;
                }
              }

              Some('e' | 'E') => {
                is_float = true;
                if is_exponent {
                  break;
                } else {
                  is_exponent = true;
                  self.tok.eat().unwrap();
                  match self.tok.peek_char() {
                    Some('+') | Some('-') => {
                      self.tok.eat().unwrap();
                    }
                    Some('0'..='9') => {}
                    Some(_) | None => break,
                  }
                }

                continue;
              }

              Some('d' | 'D') => {
                is_float = true;
                self.tok.eat().unwrap();
                break;
              }

              // TODO: Spit out an error if its already a float!
              Some('l' | 'L') => {
                is_float = false;
                self.tok.eat().unwrap();
                break;
              }

              Some(_) | None => break,
            }

            self.tok.eat().unwrap();
          }

          self.ok(start, Token::Literal(if is_float { Literal::Float } else { Literal::Integer }))
        }
      },

      InnerToken::Delimiter(Delimiter::Dot) if self.tok.peek() == Some(InnerToken::Digit) => {
        let mut is_exponent = false;
        loop {
          match self.tok.peek_char() {
            Some('0'..='9' | '_') => {}

            Some('e' | 'E') => {
              if is_exponent {
                break self.ok(start, Token::Literal(Literal::Float));
              } else {
                is_exponent = true;
                self.tok.eat().unwrap();
                match self.tok.peek_char() {
                  Some('+') | Some('-') => {
                    self.tok.eat().unwrap();
                  }
                  Some('0'..='9') => {}
                  Some(_) | None => break self.ok(start, Token::Literal(Literal::Float)),
                }
              }

              continue;
            }

            Some('d' | 'D') => {
              self.tok.eat().unwrap();
              break self.ok(start, Token::Literal(Literal::Float));
            }

            // TODO: Spit out an error!
            Some('l' | 'L') => {
              self.tok.eat().unwrap();
              break self.ok(start, Token::Literal(Literal::Float));
            }

            Some(_) | None => {
              break self.ok(start, Token::Literal(Literal::Float));
            }
          }
          self.tok.eat().unwrap();
        }
      }

      // Double quoted strings.
      InnerToken::Delimiter(Delimiter::DoubleQuote) => {
        self.ok(start, Token::Delimiter(Delimiter::DoubleQuote))
      }

      InnerToken::Delimiter(d) => self.ok(start, Token::Delimiter(d)),
      InnerToken::Group(g) => self.ok(start, Token::Group(g)),
      InnerToken::Newline => self.ok(start, Token::Newline),

      // Only shows up when in a string.
      InnerToken::Letter => self.ok(start, Token::Ident(Ident::Plain)),
      InnerToken::Operator => self.ok(start, Token::Ident(Ident::Plain)),
      InnerToken::Underscore => self.ok(start, Token::Ident(Ident::Plain)),

      t => unreachable!("inner token {t:?}"),
    }
  }

  pub fn slice(&self) -> &'a str { &self.tok.source[self.span.clone()] }

  pub fn range(&self) -> Range<usize> { self.span.clone() }
  pub fn view(&self, range: Range<usize>) -> &'a str { &self.tok.source[range] }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn plain_ident() {
    let mut lexer = Lexer::new("abc");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "abc");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc_foo");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "abc_foo");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc_++");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "abc_++");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc++");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "abc");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "++");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc_def_+");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "abc_def_+");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc_+_def");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "abc_+");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "_def");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    // This looks broken but it matches scala's behavior, so ah well.
    let mut lexer = Lexer::new("_+");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "_");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "+");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("__+");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "__+");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("foo$bar");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "foo$bar");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn operator_identifier() {
    let mut lexer = Lexer::new("++");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "++");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("+++a");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "+++");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "a");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("_ _");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "_");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "_");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("a / b");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "a");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "/");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "b");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("a ** b");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "a");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "**");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "b");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn backtick_identifier() {
    let mut lexer = Lexer::new("`hello world`");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Backtick)));
    assert_eq!(lexer.slice(), "`hello world`");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("`hello world`aa__");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Backtick)));
    assert_eq!(lexer.slice(), "`hello world`");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "aa__");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn integers() {
    let mut lexer = Lexer::new("123foo");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "123");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "foo");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("0xff");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::HexInteger)));
    assert_eq!(lexer.slice(), "0xff");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("0b012");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::BinaryInteger)));
    assert_eq!(lexer.slice(), "0b01");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "2");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("1_000_000");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "1_000_000");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn floats() {
    let mut lexer = Lexer::new("2.345");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Float)));
    assert_eq!(lexer.slice(), "2.345");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(".25");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Float)));
    assert_eq!(lexer.slice(), ".25");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("2.5e5");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Float)));
    assert_eq!(lexer.slice(), "2.5e5");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(".5e5");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Float)));
    assert_eq!(lexer.slice(), ".5e5");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("2.5e+5");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Float)));
    assert_eq!(lexer.slice(), "2.5e+5");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(".5e+5");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Float)));
    assert_eq!(lexer.slice(), ".5e+5");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("1e9");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Float)));
    assert_eq!(lexer.slice(), "1e9");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn int_dot() {
    // This requires a bit of peaking ahead shenanigins.
    let mut lexer = Lexer::new("2.foo");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "2");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::Dot)));
    assert_eq!(lexer.slice(), ".");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "foo");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn strings() {
    let mut lexer = Lexer::new("\"\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(r#" "hi" "#);
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "hi");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(
      r#" "hello
           world"
      "#,
    );
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "hello");
    assert_eq!(lexer.next(), Ok(Token::Newline));
    assert_eq!(lexer.slice(), "\n");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "           ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "world");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Newline));
    assert_eq!(lexer.slice(), "\n");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "      ");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    // Escapes.
    let mut lexer = Lexer::new("\"foo: \\\"\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "foo");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), ":");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "\\");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("\"\\\"\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "\\");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn format_strings() {
    let mut lexer = Lexer::new(r#" s"hi" "#);
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "s");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "hi");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn multiline_strings() {
    let mut lexer = Lexer::new(r#" """hi""" "#);
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "hi");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(r#" """h""i""" "#);
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "h");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "i");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(
      r#" """hello
             world"""
      "#,
    );
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "hello");
    assert_eq!(lexer.next(), Ok(Token::Newline));
    assert_eq!(lexer.slice(), "\n");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "             ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "world");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::DoubleQuote)));
    assert_eq!(lexer.slice(), "\"");
    assert_eq!(lexer.next(), Ok(Token::Newline));
    assert_eq!(lexer.slice(), "\n");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "      ");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn line_comments() {
    let mut lexer = Lexer::new(
      "3 // hi
       2 + 3",
    );
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "3");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "// hi");
    assert_eq!(lexer.next(), Ok(Token::Newline));
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "       ");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "2");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "+");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "3");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("3 // works at end");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "3");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "// works at end");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn block_comments() {
    let mut lexer = Lexer::new("3 /* hi */");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "3");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "/* hi */");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    // nested block comments
    let mut lexer = Lexer::new("3 /* hi /* foo */ */");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "3");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "/* hi /* foo */ */");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    // block comments over multiple lines
    let mut lexer = Lexer::new("3 /* hi \n */ 4");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "3");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "/* hi \n */");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "4");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn invalid_chars() {
    let mut lexer = Lexer::new("⊥");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn char_literals() {
    let mut lexer = Lexer::new("'a'");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::SingleQuote)));
    assert_eq!(lexer.slice(), "'");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "a");
    assert_eq!(lexer.next(), Ok(Token::Delimiter(Delimiter::SingleQuote)));
    assert_eq!(lexer.slice(), "'");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn whole_file() {
    let mut lexer = Lexer::new(
      "class Foo {
        def bar(): Int = 2 + 3
      }",
    );
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "class");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "Foo");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Group(Group::OpenBrace)));
    assert_eq!(lexer.slice(), "{");
    assert_eq!(lexer.next(), Ok(Token::Newline));
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "        ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "def");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "bar");
    assert_eq!(lexer.next(), Ok(Token::Group(Group::OpenParen)));
    assert_eq!(lexer.slice(), "(");
    assert_eq!(lexer.next(), Ok(Token::Group(Group::CloseParen)));
    assert_eq!(lexer.slice(), ")");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), ":");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "Int");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "=");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "2");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Operator)));
    assert_eq!(lexer.slice(), "+");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "3");
    assert_eq!(lexer.next(), Ok(Token::Newline));
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), "      ");
    assert_eq!(lexer.next(), Ok(Token::Group(Group::CloseBrace)));
    assert_eq!(lexer.slice(), "}");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }
}
