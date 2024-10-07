pub mod format;

mod grammar;
mod syntax_kind;
#[cfg(test)]
mod tests;
mod token;

use std::{mem, ops::Range};

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
  lexer: &'a mut Lexer<'a>,

  current:            SyntaxKind,
  current_range:      Range<usize>,
  pending_whitespace: usize,
  peeked_whitespace:  usize,

  events: Vec<Event>,

  peeked: Option<(SyntaxKind, Range<usize>)>,

  // Tracks the current stack of braces. This is used to disabled newlines in areas surrounded by
  // `(` and `)`, or `[` and `]`.
  //
  // NB: Braces are not matched, so if the source file has mismatched braces, then this will stop
  // working! Additionally, this operates above `peek()`, so `peek()` will still incorrectly see
  // newline tokens.
  brace_stack: Vec<Brace>,

  // If set, then braces will not be matched.
  in_string: bool,
}

#[derive(Debug)]
enum Brace {
  /// `(` and `)`
  Paren,
  /// `[` and `]`
  Bracket,
  /// `{` and `}` (poorly named, I know).
  Brace,
  /// When in a pattern for a case item (manually added by the parser).
  Pattern,
}

fn token_to_kind(token: Token, s: &str) -> SyntaxKind {
  match token {
    Token::Ident(_) => match s {
      // TODO: Keywords should be defined somewhere else.
      "package" => T![package],
      "import" => T![import],
      "def" => T![def],
      "val" => T![val],
      "var" => T![var],
      "new" => T![new],
      "case" => T![case],
      "type" => T![type],
      "class" => T![class],
      "trait" => T![trait],
      "object" => T![object],
      "extends" => T![extends],
      "implicit" => T![implicit],
      "sealed" => T![sealed],
      "with" => T![with],
      "return" => T![return],
      "throw" => T![throw],
      "private" => T![private],
      "protected" => T![protected],
      "override" => T![override],
      "lazy" => T![lazy],
      "final" => T![final],
      "abstract" => T![abstract],
      "match" => T![match],
      "true" => T![true],
      "false" => T![false],
      "if" => T![if],
      "else" => T![else],
      "while" => T![while],
      "do" => T![do],
      "for" => T![for],
      "yield" => T![yield],
      "try" => T![try],
      "catch" => T![catch],
      "finally" => T![finally],
      "macro" => T![macro],
      "=" => T![=],
      ":" => T![:],
      "=>" => T![=>],
      "<-" => T![<-],
      "<:" => T![<:],
      ">:" => T![>:],
      "@" => T![@],
      "#" => T![#],
      _ => T![ident],
    },

    // FIXME: Build different tokens for different literals.
    Token::Literal(token::Literal::Integer) => SyntaxKind::INT_LIT_KW,
    Token::Literal(token::Literal::HexInteger) => SyntaxKind::INT_LIT_KW,
    Token::Literal(token::Literal::BinaryInteger) => SyntaxKind::INT_LIT_KW,

    Token::Literal(token::Literal::Float) => SyntaxKind::FLOAT_LIT_KW,

    Token::Newline => T![nl],
    Token::Delimiter(token::Delimiter::Semicolon) => T![nl],

    Token::Whitespace => SyntaxKind::WHITESPACE,
    Token::Delimiter(token::Delimiter::Dot) => T![.],
    Token::Delimiter(token::Delimiter::Comma) => T![,],
    Token::Delimiter(token::Delimiter::SingleQuote) => SyntaxKind::SINGLE_QUOTE,
    Token::Delimiter(token::Delimiter::DoubleQuote) => SyntaxKind::DOUBLE_QUOTE,

    // Only shows up in strings.
    Token::Delimiter(token::Delimiter::Backtick) => T![ident],
    Token::Delimiter(token::Delimiter::Slash) => T![ident],
    Token::Delimiter(token::Delimiter::Backslash) => T![ident],
    Token::Delimiter(token::Delimiter::Star) => T![ident],

    Token::Group(token::Group::OpenParen) => T!['('],
    Token::Group(token::Group::CloseParen) => T![')'],
    Token::Group(token::Group::OpenBracket) => T!['['],
    Token::Group(token::Group::CloseBracket) => T![']'],
    Token::Group(token::Group::OpenBrace) => T!['{'],
    Token::Group(token::Group::CloseBrace) => T!['}'],
    _ => unreachable!("token {token:?}"),
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
    let mut p = Parser {
      current_range: 0..0,
      lexer,
      current: SyntaxKind::TOMBSTONE,
      events: Vec::new(),
      pending_whitespace: 0,
      peeked_whitespace: 0,
      peeked: None,
      in_string: false,
      brace_stack: vec![],
    };
    p.bump();
    p.events.clear(); // `bump` will push the current token, which we don't want here.
    p
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
  pub fn finish(self) -> Vec<Event> {
    #[cfg(debug_assertions)]
    if !self.events.iter().any(|e| matches!(e, Event::Error { .. })) && !self.brace_stack.is_empty()
    {
      panic!("successful parse with non-empty brace stack: {:?}", self.brace_stack);
    }

    self.events
  }

  pub fn newlines_enabled(&self) -> bool {
    match self.brace_stack.last() {
      Some(Brace::Paren | Brace::Bracket | Brace::Pattern) => false,
      Some(Brace::Brace) | None => true,
    }
  }

  pub fn set_in_string(&mut self, in_string: bool) {
    self.in_string = in_string;
    self.lexer.in_string = in_string;
  }

  fn eat_trivia(&mut self) {
    if self.pending_whitespace > 0 {
      self
        .events
        .push(Event::Token { kind: SyntaxKind::WHITESPACE, len: self.pending_whitespace });
      self.pending_whitespace = 0;
    }
  }

  pub fn peek(&mut self) -> SyntaxKind {
    if let Some((p, _)) = self.peeked {
      p
    } else {
      // Don't push an even here. Instead, we'll push `current` when we consume the
      // peeked token in `bump`.
      let t = self.bump_peek();
      self.peeked = Some((t, self.lexer.range()));
      t
    }
  }

  pub fn start(&mut self) -> Marker {
    // Special case for the first marker: put whitespace after the start, so that we
    // get a single root node.
    if !self.events.is_empty() {
      self.eat_trivia();
    }

    let i = self.events.len() as u32;
    self.events.push(Event::Start { kind: SyntaxKind::TOMBSTONE, forward_parent: None });

    if self.events.len() == 1 {
      self.eat_trivia();
    }

    Marker { pos: i, bomb: DropBomb::new("Marker must be either completed or abandoned") }
  }
  pub fn at(&mut self, t: SyntaxKind) -> bool { self.current() == t }
  pub fn current(&self) -> SyntaxKind { self.current }
  pub fn slice(&self) -> &str { self.lexer.view(self.current_range.clone()) }
  #[track_caller]
  pub fn eat(&mut self, t: SyntaxKind) {
    assert_eq!(self.current(), t, "eat got unexpected result");
    self.bump();
  }
  pub fn bump(&mut self) -> SyntaxKind {
    if !self.in_string {
      match self.current {
        T!['('] => self.brace_stack.push(Brace::Paren),
        T![')'] => {
          self.brace_stack.pop();
        }
        T!['['] => self.brace_stack.push(Brace::Bracket),
        T![']'] => {
          self.brace_stack.pop();
        }
        T!['{'] => self.brace_stack.push(Brace::Brace),
        T!['}'] => {
          self.brace_stack.pop();
        }
        _ => {}
      }
    }

    if let Some((t, r)) = self.peeked.take() {
      // Push `current`, now that we're pulling an event from `peeked`.
      self.eat_trivia();
      self.events.push(Event::Token { kind: self.current, len: self.current_range.len() });
      // TODO: Handle `semi` and `nl` correctly here.
      self.current = t;
      self.current_range = r;
      self.pending_whitespace = self.peeked_whitespace;
      self.peeked_whitespace = 0;
      t
    } else {
      let kind = self.bump_inner();
      self.current = kind;
      self.current_range = self.lexer.range();
      kind
    }
  }

  fn bump_inner(&mut self) -> SyntaxKind {
    self.eat_trivia();
    self.events.push(Event::Token { kind: self.current, len: self.lexer.slice().len() });

    loop {
      match self.lexer.next() {
        // Ignore whitespace tokens here, because we usually don't care about them when parsing. We
        // record that they got skipped, so that we can recover them later if we need a concrete
        // tree.
        Ok(Token::Whitespace) if !self.in_string => {
          self.pending_whitespace += self.lexer.slice().len();
        }
        Ok(Token::Newline) if !self.newlines_enabled() => {
          self.pending_whitespace += self.lexer.slice().len();
        }
        Ok(t) => {
          break token_to_kind(t, self.lexer.slice());
        }
        Err(LexError::EOF) => {
          break SyntaxKind::EOF;
        }
        Err(e) => {
          self.error(e.to_string());
          break self.current;
        }
      }
    }
  }

  fn bump_peek(&mut self) -> SyntaxKind {
    loop {
      match self.lexer.next() {
        // Ignore whitespace tokens here, because we usually don't care about them when parsing. We
        // record that they got skipped, so that we can recover them later if we need a concrete
        // tree.
        Ok(Token::Whitespace) if !self.in_string => {
          self.peeked_whitespace += self.lexer.slice().len();
        }
        Ok(Token::Newline) if !self.newlines_enabled() => {
          self.peeked_whitespace += self.lexer.slice().len();
        }
        Ok(t) => {
          break token_to_kind(t, self.lexer.slice());
        }
        Err(LexError::EOF) => {
          break SyntaxKind::EOF;
        }
        Err(e) => {
          self.error(e.to_string());
          break self.current;
        }
      }
    }
  }

  pub fn expect(&mut self, t: SyntaxKind) {
    if self.current() != t {
      self.error(format!("expected {t:?}"));
    } else {
      self.bump();
    }
  }

  pub fn eat_newlines(&mut self) -> u32 {
    let mut newlines = 0;
    loop {
      if self.at(T![nl]) {
        self.eat(T![nl]);
        newlines += 1;
      } else {
        break newlines;
      }
    }
  }

  fn recover_until_any(&mut self, kinds: &[SyntaxKind]) {
    while !kinds.contains(&self.current()) && self.current() != SyntaxKind::EOF {
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

// Removes all the forward_parents. TODO: Produce a new Output type or something
// to avoid exposing Event.
pub fn process_events(events: &mut [Event]) -> Vec<Event> {
  let mut out = vec![];
  let mut forward_parents = Vec::new();

  for i in 0..events.len() {
    match mem::replace(&mut events[i], Event::tombstone()) {
      Event::Start { kind, forward_parent } => {
        // For events[A, B, C], B is A's forward_parent, C is B's forward_parent,
        // in the normal control flow, the parent-child relation: `A -> B -> C`,
        // while with the magic forward_parent, it writes: `C <- B <- A`.

        // append `A` into parents.
        forward_parents.push(kind);
        let mut idx = i;
        let mut fp = forward_parent;
        while let Some(fwd) = fp {
          idx += fwd as usize;
          // append `A`'s forward_parent `B`
          fp = match mem::replace(&mut events[idx], Event::tombstone()) {
            Event::Start { kind, forward_parent } => {
              forward_parents.push(kind);
              forward_parent
            }
            _ => unreachable!(),
          };
          // append `B`'s forward_parent `C` in the next stage.
        }

        for kind in forward_parents.drain(..).rev() {
          if kind != SyntaxKind::TOMBSTONE {
            out.push(Event::Start { kind, forward_parent: None });
          }
        }
      }
      Event::Finish => {
        out.push(Event::Finish);
      }
      Event::Token { kind, len } => {
        if kind != SyntaxKind::TOMBSTONE {
          out.push(Event::Token { kind, len });
        }
      }
      Event::Error { msg } => out.push(Event::Error { msg }),
    }
  }

  out
}
