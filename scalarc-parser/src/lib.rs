use scalarc_ast::LiteralValue;
use thiserror::Error;

pub mod token;

use token::{LexError, Lexer, Token};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Error)]
pub enum ParseError {
  #[error("lexer error: {0}")]
  Lex(#[from] LexError),
}

pub struct Parser<'a> {
  lexer:  &'a mut Lexer<'a>,
  errors: Vec<ParseError>,
}

pub trait Invalid {
  fn invalid() -> Self;
}

impl Invalid for Token {
  fn invalid() -> Self { Token::Invalid }
}

impl Parser<'_> {
  pub fn parse<T: Parse>(&mut self) -> T { T::parse(self) }

  pub fn check<T, E>(&mut self, res: std::result::Result<T, E>) -> T
  where
    T: Invalid,
    E: Into<ParseError>,
  {
    match res {
      Ok(t) => t,
      Err(e) => {
        self.errors.push(e.into());
        T::invalid()
      }
    }
  }

  pub fn eat(&mut self) -> Token {
    let res = self.lexer.next();
    self.check(res)
  }
}

pub trait Parse {
  fn parse(parser: &mut Parser) -> Self
  where
    Self: Sized;
}

impl Parse for scalarc_ast::Literal {
  fn parse(parser: &mut Parser) -> Self {
    let v = match parser.eat() {
      Token::Literal(token::Literal::Float) => {
        LiteralValue::Float(parser.lexer.slice().parse().unwrap())
      }
      _ => panic!(),
    };

    scalarc_ast::Literal { span: parser.lexer.span(), value: v }
  }
}
