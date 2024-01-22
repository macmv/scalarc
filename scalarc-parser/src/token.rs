use std::ops::Range;

use scalarc_ast::Span;
use thiserror::Error;

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
  String,
}

pub type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug, Error, PartialEq)]
pub enum LexError {
  #[error("invalid character")]
  InvalidChar,

  #[error("string terminated in newline")]
  NewlineInString,

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
  Slash,
}

struct Tokenizer<'a> {
  source: &'a str,
  index:  usize,
}

impl<'a> Tokenizer<'a> {
  pub fn new(source: &'a str) -> Self { Tokenizer { source, index: 0 } }

  pub fn pos(&self) -> usize { self.index }

  pub fn peek(&mut self) -> Result<Option<InnerToken>> {
    if self.index >= self.source.len() {
      Ok(None)
    } else {
      let t = self.eat()?;
      self.index -= 1;
      Ok(Some(t))
    }
  }

  pub fn eat(&mut self) -> Result<InnerToken> {
    let t = match self.source[self.index..].chars().next() {
      Some('\u{0020}' | '\u{0009}' | '\u{000d}') => InnerToken::Whitespace,
      Some('\n') => InnerToken::Newline,

      Some('(') => InnerToken::Group(Group::OpenParen),
      Some(')') => InnerToken::Group(Group::CloseParen),
      Some('[') => InnerToken::Group(Group::OpenBracket),
      Some(']') => InnerToken::Group(Group::CloseBracket),
      Some('{') => InnerToken::Group(Group::OpenBrace),
      Some('}') => InnerToken::Group(Group::CloseBrace),

      Some('`') => InnerToken::Delimiter(Delimiter::Backtick),
      Some('\'') => InnerToken::Delimiter(Delimiter::SingleQuote),
      Some('"') => InnerToken::Delimiter(Delimiter::DoubleQuote),
      Some('.') => InnerToken::Delimiter(Delimiter::Dot),
      Some(';') => InnerToken::Delimiter(Delimiter::Semicolon),
      Some(',') => InnerToken::Delimiter(Delimiter::Comma),
      Some('/') => InnerToken::Delimiter(Delimiter::Slash),

      Some('_') => InnerToken::Underscore,
      Some('a'..='z' | 'A'..='Z') => InnerToken::Letter,
      Some('0'..='9') => InnerToken::Digit,
      Some('\u{0020}'..='\u{007e}') => InnerToken::Operator,

      Some(_) => return Err(LexError::InvalidChar),
      None => return Err(LexError::EOF),
    };
    self.index += 1;
    Ok(t)
  }

  pub fn span(&self) -> Range<usize> { self.index - 1..self.index }
}

pub struct Lexer<'a> {
  tok:  Tokenizer<'a>,
  span: Range<usize>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self { Lexer { tok: Tokenizer::new(input), span: 0..0 } }

  fn ok(&mut self, start: usize, tok: Token) -> Result<Token> {
    self.span.start = start;
    self.span.end = self.tok.span().end;
    Ok(tok)
  }

  pub fn eat_whitespace(&mut self) -> Result<Option<Token>> {
    loop {
      match self.tok.peek()? {
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
      // Plain identifier.
      InnerToken::Underscore | InnerToken::Letter => {
        // This is intentionally not `first == InnerToken::Underscore`, to match scala's
        // behavior. Honestly its probably a bug in scala. Ah well. It makes `_+` two
        // identifiers, but `__+` one identifier.
        let mut prev_was_underscore = false;
        let mut only_op = false;
        loop {
          let t = self.tok.peek()?;
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
      InnerToken::Operator => {
        loop {
          match self.tok.peek()? {
            Some(InnerToken::Operator) => {}
            Some(_) | None => break,
          }
          self.tok.eat().unwrap();
        }

        self.ok(start, Token::Ident(Ident::Operator))
      }

      // Backtick identifier.
      InnerToken::Delimiter(Delimiter::Backtick) => {
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
      InnerToken::Digit => {
        let mut is_float = false;
        loop {
          match self.tok.peek()? {
            Some(InnerToken::Digit) => {}
            Some(InnerToken::Delimiter(Delimiter::Dot)) if !is_float => is_float = true,
            Some(_) | None => break,
          }
          self.tok.eat().unwrap();
        }

        self.ok(start, Token::Literal(if is_float { Literal::Float } else { Literal::Integer }))
      }

      // Double quoted strings.
      InnerToken::Delimiter(Delimiter::DoubleQuote) => {
        let second = self.tok.eat()?;
        let third = self.tok.peek();

        if second == first && third == Ok(Some(first)) {
          // Tripple quoted strings.
          self.tok.eat()?;

          let mut quote_len = 0;

          loop {
            // TODO: Escapes
            match self.tok.eat() {
              Err(LexError::EOF) => break,
              Ok(InnerToken::Newline) => return Err(LexError::NewlineInString),
              Ok(InnerToken::Delimiter(Delimiter::DoubleQuote)) => quote_len += 1,
              Ok(_) => quote_len = 0,
              Err(e) => return Err(e),
            }

            if quote_len == 3 {
              break;
            }
          }

          self.ok(start, Token::Literal(Literal::String))
        } else if second == first {
          // Empty string.
          self.ok(start, Token::Literal(Literal::String))
        } else {
          // One-line, double quoted string.

          loop {
            // TODO: Escapes
            match self.tok.eat() {
              Ok(InnerToken::Delimiter(Delimiter::DoubleQuote)) | Err(LexError::EOF) => break,
              Ok(_) => {}
              Err(e) => return Err(e),
            }
          }

          self.ok(start, Token::Literal(Literal::String))
        }
      }

      // Line comments.
      InnerToken::Delimiter(Delimiter::Slash) if self.tok.peek() == Ok(Some(first)) => {
        self.tok.eat()?;

        loop {
          match self.tok.eat() {
            Err(LexError::EOF) => break,
            Ok(InnerToken::Newline) => break,
            Ok(_) => {}
            Err(e) => return Err(e),
          }
        }

        self.ok(start, Token::Whitespace)
      }

      InnerToken::Delimiter(d) => self.ok(start, Token::Delimiter(d)),
      InnerToken::Group(g) => self.ok(start, Token::Group(g)),
      InnerToken::Newline => self.ok(start, Token::Newline),

      _ => unreachable!(),
    }
  }

  pub fn span(&self) -> Span { self.span.clone().into() }
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
  }

  #[test]
  fn floats() {
    let mut lexer = Lexer::new("2.345");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Float)));
    assert_eq!(lexer.slice(), "2.345");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn strings() {
    let mut lexer = Lexer::new("\"\"");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::String)));
    assert_eq!(lexer.slice(), "\"\"");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(r#" "hi" "#);
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::String)));
    assert_eq!(lexer.slice(), "\"hi\"");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn format_strings() {
    let mut lexer = Lexer::new(r#" s"hi" "#);
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Ident(Ident::Plain)));
    assert_eq!(lexer.slice(), "s");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::String)));
    assert_eq!(lexer.slice(), "\"hi\"");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn multiline_strings() {
    let mut lexer = Lexer::new(r#" """hi""" "#);
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::String)));
    assert_eq!(lexer.slice(), "\"\"\"hi\"\"\"");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new(r#" """h""i""" "#);
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::String)));
    assert_eq!(lexer.slice(), "\"\"\"h\"\"i\"\"\"");
    assert_eq!(lexer.next(), Ok(Token::Whitespace));
    assert_eq!(lexer.slice(), " ");
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
    assert_eq!(lexer.slice(), "// hi\n");
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
