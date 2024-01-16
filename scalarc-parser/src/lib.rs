mod syntax_kind;
mod token;

pub use syntax_kind::SyntaxKind;
pub use token::Lexer;

pub enum EntryPoint {
  SourceFile,
}

struct Parser<'a> {
  lexer:  &'a mut Lexer<'a>,
  events: Vec<Event>,
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
  /// `n_raw_tokens` is used to glue complex contextual tokens.
  /// For example, lexer tokenizes `>>` as `>`, `>`, and
  /// `n_raw_tokens = 2` is used to produced a single `>>`.
  Token {
    kind:         SyntaxKind,
    n_raw_tokens: u8,
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

impl EntryPoint {
  pub fn parse<'a>(&'a self, lexer: &'a mut Lexer<'a>) -> Vec<Event> {
    let parser = Parser::new(lexer);
    match self {
      EntryPoint::SourceFile => {
        // grammar::entry_point::source_file(parser)
        todo!()
      }
    }
    parser.finish()
  }
}

impl<'a> Parser<'a> {
  pub fn new(lexer: &'a mut Lexer<'a>) -> Self { Parser { lexer, events: Vec::new() } }
}

impl Parser<'_> {
  pub fn finish(self) -> Vec<Event> { self.events }
}
