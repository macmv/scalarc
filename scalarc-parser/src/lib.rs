mod grammar;
mod syntax_kind;
#[cfg(test)]
mod tests;
mod token;

use std::ops::Range;

use drop_bomb::DropBomb;
pub use syntax_kind::SyntaxKind;
pub use token::Lexer;
use token::{LexError, Token};

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
#[macro_use]
extern crate scalarc_test;

pub enum EntryPoint {
  SourceFile,
  Expr,
}

struct Parser<'a> {
  lexer:              &'a mut Lexer<'a>,
  current:            SyntaxKind,
  current_range:      Range<usize>,
  pending_whitespace: usize,
  events:             Vec<Event>,
}

fn token_to_kind(token: Token, s: &str) -> SyntaxKind {
  match token {
    Token::Ident(_) => match s {
      // TODO: Keywords should be defined somewhere else.
      "import" => T![import],
      "def" => T![def],
      "=" => T![=],
      ":" => T![:],
      _ => T![ident],
    },
    Token::Literal(token::Literal::Integer) => SyntaxKind::INT_LIT_KW,
    Token::Newline => T![nl],
    Token::Whitespace => SyntaxKind::WHITESPACE,
    Token::Delimiter(token::Delimiter::Dot) => T![.],
    Token::Delimiter(token::Delimiter::Comma) => T![,],
    Token::Group(token::Group::OpenParen) => T!['('],
    Token::Group(token::Group::CloseParen) => T![')'],
    Token::Group(token::Group::OpenBrace) => T!['{'],
    Token::Group(token::Group::CloseBrace) => T!['}'],
    _ => todo!("token {token:?}"),
  }
}

/// `Parser` produces a flat list of `Event`s.
/// They are converted to a tree-structure in
/// a separate pass, via `TreeBuilder`.
#[derive(Debug)]
pub enum Event {
  /// This event signifies the start of the node.
  /// It should be either abandoned (in which case the
  /// `kind` is `TOMBSTONE`, and the event is ignored),
  /// or completed via a `Finish` event.
  ///
  /// All tokens between a `Start` and a `Finish` would
  /// become the children of the respective node.
  ///
  /// The below is all docs about `forward_parent`. I don't see a reason to add
  /// this, but I'll probably come accross it later, so I'm leaving it here.
  ///
  /// For left-recursive syntactic constructs, the parser produces
  /// a child node before it sees a parent. `forward_parent`
  /// saves the position of current event's parent.
  ///
  /// Consider this path
  ///
  /// foo::bar
  ///
  /// The events for it would look like this:
  ///
  /// ```text
  /// START(PATH) IDENT('foo') FINISH START(PATH) T![::] IDENT('bar') FINISH
  ///       |                          /\
  ///       |                          |
  ///       +------forward-parent------+
  /// ```
  ///
  /// And the tree would look like this
  ///
  /// ```text
  ///    +--PATH---------+
  ///    |   |           |
  ///    |   |           |
  ///    |  '::'       'bar'
  ///    |
  ///   PATH
  ///    |
  ///   'foo'
  /// ```
  ///
  /// See also `CompletedMarker::precede`.
  Start {
    kind:           SyntaxKind,
    forward_parent: Option<u32>,
  },

  /// Complete the previous `Start` event
  Finish,

  /// Produce a single leaf-element.
  Token {
    kind: SyntaxKind,
    len:  usize,
  },
  /// When we parse `foo.0.0` or `foo. 0. 0` the lexer will hand us a float
  /// literal instead of an integer literal followed by a dot as the lexer has
  /// no contextual knowledge. This event instructs whatever consumes the
  /// events to split the float literal into the corresponding parts.
  FloatSplitHack {
    ends_in_dot: bool,
  },
  Error {
    msg: String,
  },
}
impl Event {
  pub fn tombstone() -> Self { Event::Token { kind: SyntaxKind::TOMBSTONE, len: 0 } }
}

impl EntryPoint {
  pub fn parse<'a>(&'a self, lexer: &'a mut Lexer<'a>) -> Vec<Event> {
    let mut parser = Parser::new(lexer);
    match self {
      EntryPoint::SourceFile => grammar::entry_point::source_file(&mut parser),
      EntryPoint::Expr => grammar::entry_point::expr(&mut parser),
    }
    parser.finish()
  }
}

impl<'a> Parser<'a> {
  pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
    let first = token_to_kind(lexer.next().unwrap(), lexer.slice());

    Parser {
      current_range: lexer.range(),
      lexer,
      current: first,
      events: Vec::new(),
      pending_whitespace: 0,
    }
  }
}

struct Marker {
  pos:  u32,
  bomb: DropBomb,
}

struct CompletedMarker {
  pos: u32,
}

impl Parser<'_> {
  pub fn finish(self) -> Vec<Event> { self.events }

  fn eat_trivia(&mut self) {
    if self.pending_whitespace > 0 {
      self
        .events
        .push(Event::Token { kind: SyntaxKind::WHITESPACE, len: self.pending_whitespace });
      self.pending_whitespace = 0;
    }
  }

  pub fn start(&mut self) -> Marker {
    self.eat_trivia();

    let i = self.events.len() as u32;
    self.events.push(Event::Start { kind: SyntaxKind::TOMBSTONE, forward_parent: None });
    Marker { pos: i, bomb: DropBomb::new("Marker must be either completed or abandoned") }
  }
  pub fn at(&mut self, t: SyntaxKind) -> bool { self.current() == t }
  pub fn current(&self) -> SyntaxKind { self.current }
  pub fn slice(&self) -> &str { &self.lexer.view(self.current_range.clone()) }
  #[track_caller]
  pub fn eat(&mut self, t: SyntaxKind) {
    assert_eq!(self.current(), t);
    self.bump();
  }
  pub fn bump(&mut self) -> SyntaxKind {
    self.eat_trivia();
    self.events.push(Event::Token { kind: self.current, len: self.lexer.slice().len() });

    loop {
      match self.lexer.next() {
        // Ignore whitespace tokens here, because we usually don't care about them when parsing. We
        // record that they got skipped, so that we can recover them later if we need a concrete
        // tree.
        Ok(Token::Whitespace) => {
          self.pending_whitespace += 1;
        }
        Ok(t) => {
          self.current = token_to_kind(t, self.lexer.slice());
          self.current_range = self.lexer.range();
          break;
        }
        Err(LexError::EOF) => {
          self.current = SyntaxKind::EOF;
          self.current_range = self.lexer.range();
          break;
        }
        Err(e) => {
          self.error(e.to_string());
          break;
        }
      }
    }
    self.current
  }
  pub fn expect(&mut self, t: SyntaxKind) {
    if self.current() != t {
      self.error(format!("expected {t:?}"));
    } else {
      self.bump();
    }
  }

  fn recover_until(&mut self, t: SyntaxKind) {
    while self.current() != t && self.current() != SyntaxKind::EOF {
      self.bump();
    }
  }

  pub fn error_bump(&mut self, msg: impl Into<String>) {
    self.error(msg);
    self.bump();
  }
  pub fn error(&mut self, msg: impl Into<String>) {
    self.events.push(Event::Error { msg: msg.into() })
  }
}

impl Marker {
  pub fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
    self.bomb.defuse();
    match &mut parser.events[self.pos as usize] {
      Event::Start { kind: k, .. } => *k = kind,
      _ => unreachable!(),
    }
    parser.events.push(Event::Finish);
    CompletedMarker { pos: self.pos }
  }
  pub fn abandon(mut self, parser: &mut Parser) {
    self.bomb.defuse();

    #[cfg(debug_assertions)]
    match parser.events[self.pos as usize] {
      Event::Start { kind: SyntaxKind::TOMBSTONE, forward_parent: None } => (),
      _ => unreachable!(),
    }

    if self.pos as usize == parser.events.len() - 1 {
      match parser.events.pop() {
        // Sanity check
        Some(Event::Start { kind: SyntaxKind::TOMBSTONE, forward_parent: None }) => (),
        _ => unreachable!(),
      }
    }
  }
}

impl CompletedMarker {
  fn precede(self, p: &mut Parser) -> Marker {
    let new_pos = p.start();
    let idx = self.pos as usize;
    match &mut p.events[idx] {
      Event::Start { forward_parent, .. } => {
        *forward_parent = Some(new_pos.pos - self.pos);
      }
      _ => unreachable!(),
    }
    new_pos
  }
}
