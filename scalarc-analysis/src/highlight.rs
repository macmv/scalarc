use scalarc_hir::{DefinitionKind, HirDatabase};
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

  // String literals.
  String,

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
      hl.visit(item);
    }

    hl.hl
  }
}

impl<'a> Highlighter<'a> {
  pub fn new(db: &'a dyn HirDatabase, file: FileId) -> Self {
    Highlighter { db, file, hl: Highlight { tokens: vec![] } }
  }

  pub fn visit<T: Highlightable>(&mut self, node: T) { node.highlight(self); }

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

trait Highlightable {
  fn highlight(&self, h: &mut Highlighter);
}

impl<T> Highlightable for Option<T>
where
  T: Highlightable,
{
  fn highlight(&self, h: &mut Highlighter) {
    if let Some(v) = self {
      v.highlight(h);
    }
  }
}

impl Highlightable for ast::Item {
  fn highlight(&self, h: &mut Highlighter) {
    match self {
      ast::Item::ExprItem(e) => {
        if let Some(e) = e.expr() {
          h.visit(e);
        }
      }
      ast::Item::ClassDef(o) => {
        h.highlight_opt(o.case_token(), HighlightKind::Keyword);
        h.highlight_opt(o.class_token(), HighlightKind::Keyword);
        h.highlight_opt(o.id_token(), HighlightKind::Class);

        h.visit(o.body());
      }

      ast::Item::ValDef(d) => {
        h.highlight_opt(d.val_token(), HighlightKind::Keyword);
        h.highlight_opt(d.id_token(), HighlightKind::Variable);
        h.highlight_opt(d.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);

        if let Some(body) = d.expr() {
          h.visit(body);
        }
      }

      ast::Item::FunDef(d) => {
        h.highlight_opt(d.def_token(), HighlightKind::Keyword);

        if let Some(sig) = d.fun_sig() {
          h.highlight_opt(sig.id_token(), HighlightKind::Function);

          for params in sig.fun_paramss() {
            for param in params.fun_params() {
              h.highlight_opt(param.id_token(), HighlightKind::Parameter);
              h.highlight_opt(param.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);
            }
          }

          h.highlight_opt(sig.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);
        }

        if let Some(body) = d.expr() {
          h.visit(body);
        }
      }
      _ => {}
    }
  }
}

impl Highlightable for ast::Expr {
  fn highlight(&self, h: &mut Highlighter) {
    match self {
      ast::Expr::IdentExpr(id) => {
        if let Some(id) = id.id_token() {
          // FIXME: This isn't particularly efficient. However, the scopes for the whole
          // file will get cached, so its not all that bad.
          let def = h.db.def_at_index(h.file, id.text_range().start());

          if let Some(def) = def {
            h.highlight(
              id.text_range(),
              match def.kind {
                DefinitionKind::Val(_) => HighlightKind::Variable,
                DefinitionKind::Var => HighlightKind::Variable,
                DefinitionKind::Parameter => HighlightKind::Parameter,
                DefinitionKind::Def(_) => HighlightKind::Function,
                DefinitionKind::Class(_) => HighlightKind::Class,
              },
            );
          }
        }
      }
      ast::Expr::LitExpr(lit) => {
        h.highlight(lit.syntax().text_range(), HighlightKind::Number);
      }
      ast::Expr::DoubleQuotedString(d) => {
        h.highlight(d.syntax().text_range(), HighlightKind::String);
      }
      ast::Expr::BlockExpr(b) => {
        for item in b.items() {
          h.visit(item);
        }
      }
      ast::Expr::InfixExpr(i) => {
        if let Some(lhs) = i.lhs() {
          h.visit(lhs);
        }
        h.highlight_opt(i.id_token(), HighlightKind::Function);
        if let Some(rhs) = i.rhs() {
          h.visit(rhs);
        }
      }
      ast::Expr::CallExpr(c) => {
        if let Some(fun) = c.expr() {
          h.visit(fun);
        }

        if let Some(args) = c.arguments() {
          match args {
            ast::Arguments::ParenArguments(p) => {
              for arg in p.exprs() {
                h.visit(arg);
              }
            }
            _ => {}
          }
        }
      }
      ast::Expr::IfExpr(i) => {
        h.highlight_opt(i.if_token(), HighlightKind::Keyword);

        if let Some(cond) = i.cond() {
          h.visit(cond);
        }
        if let Some(then) = i.then() {
          h.visit(then);
        }
        if let Some(else_branch) = i.els() {
          h.visit(else_branch);
        }
      }
      ast::Expr::MatchExpr(m) => {
        if let Some(e) = m.expr() {
          h.visit(e);
        }
        h.highlight_opt(m.match_token(), HighlightKind::Keyword);

        for case in m.case_items() {
          h.highlight_opt(case.case_token(), HighlightKind::Keyword);

          if let Some(pat) = case.pattern() {
            h.visit(pat);
          }
          if let Some(guard) = case.guard() {
            h.highlight_opt(guard.if_token(), HighlightKind::Keyword);
            if let Some(expr) = guard.expr() {
              h.visit(expr);
            }
          }

          if let Some(b) = case.block() {
            for item in b.items() {
              h.visit(item);
            }
          }
        }
      }
      ast::Expr::NewExpr(n) => {
        h.highlight_opt(n.new_token(), HighlightKind::Keyword);

        if let Some(args) = n.paren_arguments() {
          for arg in args.exprs() {
            h.visit(arg);
          }
        }

        if let Some(block) = n.block_expr() {
          for item in block.items() {
            h.visit(item);
          }
        }
      }
      _ => {}
    }
  }
}

impl Highlightable for ast::Pattern {
  fn highlight(&self, h: &mut Highlighter) {
    match self {
      ast::Pattern::IdentPattern(i) => {
        h.highlight_opt(i.id_token(), HighlightKind::Variable);
      }
      ast::Pattern::TypePattern(i) => {
        h.highlight_opt(i.id_token(), HighlightKind::Variable);

        if let Some(ty) = i.ty() {
          h.highlight(ty.syntax().text_range(), HighlightKind::Type);
        }
      }

      _ => {}
    }
  }
}

impl Highlightable for ast::ItemBody {
  fn highlight(&self, h: &mut Highlighter) {
    for item in self.items() {
      h.visit(item);
    }
  }
}
