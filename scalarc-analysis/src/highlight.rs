use scalarc_hir::{DefinitionKind, GlobalDefinition, HirDatabase, LocalDefinition};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode},
  node::SyntaxToken,
  TextRange,
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

  /// Object names and references.
  Object,

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

struct Highlighter<'a> {
  db:   &'a dyn HirDatabase,
  file: FileId,

  hl: Highlight,
}

impl Highlight {
  pub fn from_ast(db: &dyn HirDatabase, file: FileId) -> Highlight {
    let mut hl = Highlighter::new(db, file);

    let ast = db.parse(file);

    for item in ast.tree().items() {
      hl.visit_item(item);
    }

    hl.hl
  }
}

impl<'a> Highlighter<'a> {
  pub fn new(db: &'a dyn HirDatabase, file: FileId) -> Self {
    Highlighter { db, file, hl: Highlight { tokens: vec![] } }
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

      ast::Item::ValDef(d) => {
        self.highlight_opt(d.val_token(), HighlightKind::Keyword);
        self.highlight_opt(d.id_token(), HighlightKind::Variable);
        self.highlight_opt(d.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);

        if let Some(body) = d.expr() {
          self.visit_expr(body);
        }
      }

      ast::Item::FunDef(d) => {
        self.highlight_opt(d.def_token(), HighlightKind::Keyword);

        if let Some(sig) = d.fun_sig() {
          self.highlight_opt(sig.id_token(), HighlightKind::Function);

          for params in sig.fun_paramss() {
            for param in params.fun_params() {
              self.highlight_opt(param.id_token(), HighlightKind::Parameter);
              self.highlight_opt(param.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);
            }
          }

          self.highlight_opt(sig.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);
        }

        if let Some(body) = d.expr() {
          self.visit_expr(body);
        }
      }
      _ => {}
    }
  }

  pub fn visit_expr(&mut self, expr: ast::Expr) {
    info!("visit_expr: {:?}", expr);
    match expr {
      ast::Expr::IdentExpr(id) => {
        if let Some(id) = id.id_token() {
          // FIXME: This isn't particularly efficient. However, the scopes for the whole
          // file will get cached, so its not all that bad.
          let def = self.db.def_at_index(self.file, id.text_range().start());

          if let Some(def) = def {
            self.highlight(
              id.text_range(),
              match def.kind {
                DefinitionKind::Local(LocalDefinition::Val) => HighlightKind::Variable,
                DefinitionKind::Local(LocalDefinition::Var) => HighlightKind::Variable,
                DefinitionKind::Local(LocalDefinition::Parameter) => HighlightKind::Parameter,
                DefinitionKind::Local(LocalDefinition::Def) => HighlightKind::Function,
                DefinitionKind::Global(GlobalDefinition::Class) => HighlightKind::Class,
                DefinitionKind::Global(GlobalDefinition::Object) => HighlightKind::Object,
              },
            );
          }
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
      ast::Expr::IfExpr(i) => {
        self.highlight_opt(i.if_token(), HighlightKind::Keyword);

        if let Some(cond) = i.cond() {
          self.visit_expr(cond);
        }
        if let Some(then) = i.then() {
          self.visit_expr(then);
        }
        if let Some(else_branch) = i.els() {
          self.visit_expr(else_branch);
        }
      }
      ast::Expr::MatchExpr(m) => {
        if let Some(e) = m.expr() {
          self.visit_expr(e);
        }
        self.highlight_opt(m.match_token(), HighlightKind::Keyword);

        for case in m.case_items() {
          self.highlight_opt(case.case_token(), HighlightKind::Keyword);

          if let Some(pat) = case.pattern() {
            self.visit_pattern(pat);
          }
          if let Some(guard) = case.guard() {
            self.highlight_opt(guard.if_token(), HighlightKind::Keyword);
            if let Some(expr) = guard.expr() {
              self.visit_expr(expr);
            }
          }

          if let Some(b) = case.block() {
            for item in b.items() {
              self.visit_item(item);
            }
          }
        }
      }
      _ => {}
    }
  }

  fn visit_pattern(&mut self, pat: ast::Pattern) {
    match pat {
      ast::Pattern::IdentPattern(i) => {
        self.highlight_opt(i.id_token(), HighlightKind::Variable);
      }
      ast::Pattern::TypePattern(i) => {
        self.highlight_opt(i.id_token(), HighlightKind::Variable);

        if let Some(ty) = i.ty() {
          self.highlight(ty.syntax().text_range(), HighlightKind::Type);
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
