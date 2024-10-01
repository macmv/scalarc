use scalarc_hir::{DefinitionKind, HirDatabase, Path};
use scalarc_source::{FileId, TargetId};
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

  /// Trait names and references.
  Trait,

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
  db:     &'a dyn HirDatabase,
  file:   FileId,
  target: Option<TargetId>,

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
    Highlighter { db, file, target: db.file_target(file), hl: Highlight { tokens: vec![] } }
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
        h.visit(e.expr());
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

        h.visit(d.expr());
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

        h.visit(d.expr());
      }

      ast::Item::Import(i) => {
        h.highlight_opt(i.import_token(), HighlightKind::Keyword);

        for expr in i.import_exprs() {
          h.visit(expr);
        }
      }

      _ => {}
    }
  }
}

impl Highlightable for ast::ImportExpr {
  fn highlight(&self, h: &mut Highlighter) {
    match self {
      ast::ImportExpr::Path(p) => {
        let mut path = Path::new();

        for id in p.ids() {
          path.elems.push(id.text().into());
        }

        if let Some(target) = h.target {
          let def = match h
            .db
            .definition_for_key(target, scalarc_hir::DefinitionKey::Instance(path.clone()))
          {
            Some(def) => def,
            None => {
              match h
                .db
                .definition_for_key(target, scalarc_hir::DefinitionKey::Object(path.clone()))
              {
                Some(def) => def,
                None => return,
              }
            }
          };

          let kind = match def.kind {
            DefinitionKind::Class(_) => HighlightKind::Class,
            DefinitionKind::Trait(_) => HighlightKind::Trait,
            DefinitionKind::Object(_) => HighlightKind::Object,
            _ => return,
          };

          if let Some(id) = p.ids().last() {
            h.highlight(id.text_range(), kind);
          }
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
                DefinitionKind::Trait(_) => HighlightKind::Trait,
                DefinitionKind::Object(_) => HighlightKind::Object,
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
        h.visit(i.lhs());
        h.highlight_opt(i.id_token(), HighlightKind::Function);
        h.visit(i.rhs());
      }
      ast::Expr::CallExpr(c) => {
        h.visit(c.expr());

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

        h.visit(i.cond());
        h.visit(i.then());
        h.visit(i.els());
      }
      ast::Expr::MatchExpr(m) => {
        h.visit(m.expr());
        h.highlight_opt(m.match_token(), HighlightKind::Keyword);

        for case in m.case_items() {
          h.highlight_opt(case.case_token(), HighlightKind::Keyword);

          h.visit(case.pattern());
          if let Some(guard) = case.guard() {
            h.highlight_opt(guard.if_token(), HighlightKind::Keyword);
            h.visit(guard.expr());
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
      ast::Pattern::PathPattern(_) => {
        // h.highlight_opt(i.path(), HighlightKind::Variable);
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
