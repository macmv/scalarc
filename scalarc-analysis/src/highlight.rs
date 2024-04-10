use scalarc_syntax::{
  ast::{self, AstNode},
  node::SyntaxToken,
  Parse, SourceFile, TextRange,
};

#[derive(Debug, Clone)]
pub struct Highlight {
  pub tokens: Vec<HighlightToken>,
}

#[derive(Debug, Clone)]
pub struct HighlightToken {
  pub range:      TextRange,
  pub kind:       HighlightKind,
  pub modifierst: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum HighlightKind {
  /// Class names and references.
  Class,

  /// Function calls and definitions.
  Function,

  /// Keywords like `class` or `val`.
  Keyword,

  /// Number literals.
  Number,

  /// Parameters in function definitions, like the `x` in `def foo(x: Int)`.
  Parameter,

  /// Type references, like the `Int` in `val x: Int = 92` or `def foo(x: Int)`.
  Type,

  /// Local variables.
  // Keep last!
  Variable,
}

struct Highlighter {
  // Stack of scopes.
  frames: Vec<usize>,
  // Arguments in the current scope.
  params: Vec<String>,

  hl: Highlight,
}

impl Highlight {
  pub fn from_ast(ast: Parse<SourceFile>) -> Highlight {
    let mut hl = Highlighter::new();

    for item in ast.tree().items() {
      hl.visit_item(item);
    }

    hl.hl
  }
}

// FIXME: Replace with HIR
impl Highlighter {
  pub fn new() -> Self {
    Highlighter { params: vec![], frames: vec![], hl: Highlight { tokens: vec![] } }
  }

  pub fn push(&mut self) { self.frames.push(self.params.len()); }

  pub fn pop(&mut self) {
    let idx = self.frames.pop().expect("empty stack");
    self.params.truncate(idx);
  }

  pub fn visit_item(&mut self, item: ast::Item) {
    match item {
      ast::Item::ExprItem(e) => {
        if let Some(e) = e.expr() {
          self.visit_expr(e);
        }
      }
      ast::Item::ClassDef(o) => {
        self.highlight_opt(o.case_token(), HighlightKind::Keyword);
        self.highlight_opt(o.class_token(), HighlightKind::Keyword);
        self.highlight_opt(o.id_token(), HighlightKind::Class);

        self.visit_body(o.body());
      }
      ast::Item::FunDef(d) => {
        self.highlight_opt(d.def_token(), HighlightKind::Keyword);

        self.push();

        if let Some(sig) = d.fun_sig() {
          self.highlight_opt(sig.id_token(), HighlightKind::Function);

          if let Some(params) = sig.fun_params() {
            for param in params.fun_params() {
              if let Some(name) = param.id_token() {
                self.params.push(name.text().to_string());
              }

              self.highlight_opt(param.id_token(), HighlightKind::Parameter);
              self.highlight_opt(param.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);
            }
          }

          self.highlight_opt(sig.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);
        }

        if let Some(body) = d.expr() {
          self.visit_expr(body);
        }

        self.pop();
      }
      _ => {}
    }
  }

  pub fn visit_expr(&mut self, expr: ast::Expr) {
    info!("visit_expr: {:?}", expr);
    match expr {
      ast::Expr::IdentExpr(id) => {
        if let Some(id) = id.id_token() {
          self.highlight(
            id.text_range(),
            if self.params.contains(&id.text().to_string()) {
              HighlightKind::Parameter
            } else {
              HighlightKind::Variable
            },
          );
        }
      }
      ast::Expr::LitExpr(lit) => {
        self.highlight(lit.syntax().text_range(), HighlightKind::Number);
      }
      ast::Expr::BlockExpr(b) => {
        for item in b.items() {
          self.visit_item(item);
        }
      }
      ast::Expr::InfixExpr(i) => {
        info!("infix: {:#?}", i.syntax());
        if let Some(lhs) = i.lhs() {
          self.visit_expr(lhs);
        }
        self.highlight_opt(i.id_token(), HighlightKind::Function);
        if let Some(rhs) = i.rhs() {
          self.visit_expr(rhs);
        }
      }
      ast::Expr::CallExpr(c) => {
        if let Some(fun) = c.expr() {
          self.visit_expr(fun);
        }

        if let Some(args) = c.arguments() {
          match args {
            ast::Arguments::ParenArguments(p) => {
              for arg in p.exprs() {
                self.visit_expr(arg);
              }
            }
            _ => {}
          }
        }
      }
      _ => {}
    }
  }

  fn visit_body(&mut self, body: Option<ast::ItemBody>) {
    if let Some(body) = body {
      for item in body.items() {
        self.visit_item(item);
      }
    }
  }

  pub fn highlight_opt<T: TextRangeable>(&mut self, token: Option<T>, kind: HighlightKind) {
    if let Some(token) = token {
      self.highlight(token, kind);
    }
  }

  pub fn highlight<T: TextRangeable>(
    &mut self,
    token: T,
    kind: HighlightKind,
  ) -> &mut HighlightToken {
    self.hl.tokens.push(HighlightToken { range: token.text_range(), kind, modifierst: 0 });
    self.hl.tokens.last_mut().unwrap()
  }
}

trait TextRangeable {
  fn text_range(&self) -> TextRange;
}

impl TextRangeable for SyntaxToken {
  fn text_range(&self) -> TextRange { self.text_range() }
}

impl TextRangeable for TextRange {
  fn text_range(&self) -> TextRange { *self }
}

impl HighlightKind {
  pub fn iter() -> impl Iterator<Item = HighlightKind> {
    (0..=HighlightKind::Variable as u8).map(|i| unsafe { std::mem::transmute(i) })
  }
}
