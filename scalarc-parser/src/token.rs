use std::ops::Range;

// Scala's grammar is quite finicky. We expose a `Token` enum that is a
// parser-usable version of a token. It contains high level concepts like
// "Identifiers" and "Numbers".
//
// In order to actually parse identifiers (of all things), we have a separate
// inner token type, which implements the various parts of an identifier.

#[derive(Debug, PartialEq)]
pub enum Token {
  Identifier,

  Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
  Integer,
  Float,
  String,
}

pub type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug, PartialEq)]
pub enum LexError {
  InvalidChar,

  EOF,
}

// Below we have the lexer internals.

#[derive(Clone, Debug, PartialEq)]
enum InnerToken {
  Whitespace,

  Underscore,
  Letter,
  Digit,
  Operator,

  Group(Group),
  Delimiter(Delimiter),
}

#[derive(Clone, Debug, PartialEq)]
enum Group {
  OpenParen,
  CloseParen,

  OpenBracket,
  CloseBracket,

  OpenBrace,
  CloseBrace,
}

#[derive(Clone, Debug, PartialEq)]
enum Delimiter {
  Backtick,
  SingleQuote,
  Dot,
  Semicolon,
  Comma,
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
      Some('\u{0020}' | '\u{0009}' | '\u{000d}' | '\u{000a}') => InnerToken::Whitespace,

      Some('(') => InnerToken::Group(Group::OpenParen),
      Some(')') => InnerToken::Group(Group::CloseParen),
      Some('[') => InnerToken::Group(Group::OpenBracket),
      Some(']') => InnerToken::Group(Group::CloseBracket),
      Some('{') => InnerToken::Group(Group::OpenBrace),
      Some('}') => InnerToken::Group(Group::CloseBrace),

      Some('`') => InnerToken::Delimiter(Delimiter::Backtick),
      Some('\'') => InnerToken::Delimiter(Delimiter::SingleQuote),
      Some('.') => InnerToken::Delimiter(Delimiter::Dot),
      Some(';') => InnerToken::Delimiter(Delimiter::Semicolon),
      Some(',') => InnerToken::Delimiter(Delimiter::Comma),

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

  fn eat_whitespace(&mut self) -> Result<()> {
    loop {
      match self.tok.peek()? {
        Some(InnerToken::Whitespace) => {}
        Some(_) | None => break,
      }
      self.tok.eat().unwrap();
    }
    Ok(())
  }

  pub fn next(&mut self) -> Result<Token> {
    self.eat_whitespace()?;

    let start = self.tok.pos();
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

        self.ok(start, Token::Identifier)
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

        self.ok(start, Token::Identifier)
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

        self.ok(start, Token::Identifier)
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

      _ => unreachable!(),
    }
  }

  pub fn slice(&self) -> &'a str { &self.tok.source[self.span.clone()] }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn plain_ident() {
    let mut lexer = Lexer::new("abc");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "abc");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc_foo");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "abc_foo");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc_++");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "abc_++");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc++");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "abc");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "++");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc_def_+");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "abc_def_+");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("abc_+_def");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "abc_+");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "_def");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    // This looks broken but it matches scala's behavior, so ah well.
    let mut lexer = Lexer::new("_+");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "_");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "+");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("__+");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "__+");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn operator_identifier() {
    let mut lexer = Lexer::new("++");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "++");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("+++a");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "+++");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "a");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("_ _");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "_");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "_");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn backtick_identifier() {
    let mut lexer = Lexer::new("`hello world`");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "`hello world`");
    assert_eq!(lexer.next(), Err(LexError::EOF));

    let mut lexer = Lexer::new("`hello world`aa__");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "`hello world`");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
    assert_eq!(lexer.slice(), "aa__");
    assert_eq!(lexer.next(), Err(LexError::EOF));
  }

  #[test]
  fn integers() {
    let mut lexer = Lexer::new("123foo");
    assert_eq!(lexer.next(), Ok(Token::Literal(Literal::Integer)));
    assert_eq!(lexer.slice(), "123");
    assert_eq!(lexer.next(), Ok(Token::Identifier));
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
}
