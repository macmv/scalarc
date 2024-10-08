use scalarc_hir::{
  hir::{self, AstId, BindingKind, BlockId},
  AnyDefinition, DefinitionKey, GlobalDefinition, GlobalDefinitionKind, HirDatabase,
  HirDefinitionKind, InFileExt, Path, Type,
};
use scalarc_source::{FileId, TargetId};
use scalarc_syntax::{
  ast::{self, AstNode},
  node::SyntaxToken,
  AstPtr, SyntaxNodePtr, TextRange,
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

    let syntax = ast.syntax_node();
    for node in syntax.descendants() {
      if let Some(node) = ast::Expr::cast(node.clone()) {
        hl.visit(node);
      } else if let Some(node) = ast::Pattern::cast(node.clone()) {
        hl.visit(node);
      } else if let Some(node) = ast::Item::cast(node) {
        hl.visit(node);
      }
    }

    hl.hl.tokens.sort_by_key(|t| t.range.start());

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
      ast::Item::ClassDef(o) => {
        for modifier in o.modifiers() {
          h.highlight_opt(modifier.final_token(), HighlightKind::Keyword);
          h.highlight_opt(modifier.abstract_token(), HighlightKind::Keyword);
        }
        h.highlight_opt(o.case_token(), HighlightKind::Keyword);
        h.highlight_opt(o.class_token(), HighlightKind::Keyword);
        h.highlight_opt(o.id_token(), HighlightKind::Class);
      }
      ast::Item::ObjectDef(o) => {
        h.highlight_opt(o.case_token(), HighlightKind::Keyword);
        h.highlight_opt(o.object_token(), HighlightKind::Keyword);
        h.highlight_opt(o.id_token(), HighlightKind::Object);
      }
      ast::Item::TraitDef(t) => {
        for modifier in t.modifiers() {
          h.highlight_opt(modifier.sealed_token(), HighlightKind::Keyword);
        }
        h.highlight_opt(t.trait_token(), HighlightKind::Keyword);
        h.highlight_opt(t.id_token(), HighlightKind::Trait);
      }

      ast::Item::ValDef(d) => {
        for modifier in d.modifiers() {
          h.highlight_opt(modifier.override_token(), HighlightKind::Keyword);
          h.highlight_opt(modifier.implicit_token(), HighlightKind::Keyword);
        }
        h.highlight_opt(d.val_token(), HighlightKind::Keyword);
        h.highlight_opt(d.id_token(), HighlightKind::Variable);
        h.highlight_opt(d.ty().map(|v| v.syntax().text_range()), HighlightKind::Type);
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

fn def_for_path(h: &Highlighter, path: &Path) -> Option<GlobalDefinition> {
  if let Some(target) = h.target {
    match h.db.definition_for_key(target, scalarc_hir::DefinitionKey::Instance(path.clone())) {
      Some(def) => Some(def),
      None => h.db.definition_for_key(target, scalarc_hir::DefinitionKey::Object(path.clone())),
    }
  } else {
    None
  }
}

fn kind_for_def(def: &GlobalDefinition) -> Option<HighlightKind> {
  match def.kind {
    GlobalDefinitionKind::Class(_, _) => Some(HighlightKind::Class),
    GlobalDefinitionKind::Trait(_) => Some(HighlightKind::Trait),
    GlobalDefinitionKind::Object(_) => Some(HighlightKind::Object),
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

        if let Some(def) = def_for_path(h, &path) {
          if let Some(kind) = kind_for_def(&def) {
            if let Some(id) = p.ids().last() {
              h.highlight(id.text_range(), kind);
            }
          }
        }
      }
      ast::ImportExpr::ImportSelectors(i) => {
        let mut prefix = Path::new();

        if let Some(p) = i.path() {
          for id in p.ids() {
            prefix.elems.push(id.text().into());
          }
        }

        for sel in i.import_selectors() {
          match sel {
            ast::ImportSelector::ImportSelectorId(s) => {
              if let Some(id) = s.id_token() {
                let mut path = prefix.clone();
                path.elems.push(s.id_token().unwrap().text().into());

                if let Some(def) = def_for_path(h, &path) {
                  if let Some(kind) = kind_for_def(&def) {
                    h.highlight(id.text_range(), kind);
                  }
                }
              }
            }
            _ => {}
          }
        }
      }
    }
  }
}

impl Highlightable for ast::Expr {
  fn highlight(&self, h: &mut Highlighter) {
    let block = h.db.block_for_node(SyntaxNodePtr::new(self.syntax()).in_file(h.file));
    let (hir, source_map) = h.db.hir_ast_with_source_for_block(block);

    let Some(id) = source_map.expr(AstPtr::new(self)) else { return };

    match hir.exprs[id] {
      hir::Expr::Name(_) => {
        let ast::Expr::IdentExpr(e) = self else { return };

        let def = h.db.def_for_expr(block, id);

        if let Some(def) = def {
          h.highlight(
            e.id_token().unwrap().text_range(),
            match def {
              AnyDefinition::Hir(hir) => match hir.kind {
                HirDefinitionKind::Val(_) => HighlightKind::Variable,
                HirDefinitionKind::Var(_) => HighlightKind::Variable,
                HirDefinitionKind::Def(_) => HighlightKind::Function,
                HirDefinitionKind::Parameter(_) => HighlightKind::Parameter,
                HirDefinitionKind::Pattern => HighlightKind::Variable,
                HirDefinitionKind::Import => HighlightKind::Class,
              },
              AnyDefinition::Global(def) => match def.kind {
                GlobalDefinitionKind::Class(_, _) => HighlightKind::Class,
                GlobalDefinitionKind::Trait(_) => HighlightKind::Trait,
                GlobalDefinitionKind::Object(_) => HighlightKind::Object,
              },
            },
          );
        }
      }
      hir::Expr::Literal(hir::Literal::Int(_) | hir::Literal::Long(_) | hir::Literal::Float(_)) => {
        h.highlight(self.syntax().text_range(), HighlightKind::Number);
      }
      hir::Expr::Literal(hir::Literal::Char(_) | hir::Literal::String(_)) => {
        h.highlight(self.syntax().text_range(), HighlightKind::String);
      }

      hir::Expr::InterpolatedString(_) => {
        let ast::Expr::InterpolatedString(string) = self else { return };

        let start = string.syntax().text_range().start();
        let mut prev = start;

        for intp in string.block_exprs() {
          if intp.syntax().text_range().start() != prev {
            h.highlight(
              TextRange::new(prev, intp.syntax().text_range().start()),
              HighlightKind::String,
            );
          }

          if let Some(tok) = intp.syntax().first_token() {
            if tok.text() == "$" {
              h.highlight(tok.text_range(), HighlightKind::Keyword);
            }
          }

          prev = intp.syntax().text_range().end();
        }

        if prev != string.syntax().text_range().end() {
          h.highlight(
            TextRange::new(prev, string.syntax().text_range().end()),
            HighlightKind::String,
          );
        }
      }

      hir::Expr::FieldAccess(lhs, ref name) => {
        let ast::Expr::FieldExpr(f) = self else { return };

        let Some(lhs_ty) = h.db.type_of_expr(block, lhs) else { return };
        let Some(target) = h.db.file_target(block.file_id) else { return };
        let Some(def) = def_for_ty(h.db, target, lhs_ty) else { return };

        let field_ty = select_name_from_def(h.db, def, name);
        if field_ty.is_some() {
          h.highlight(f.id_token().unwrap().text_range(), HighlightKind::Function);
        }
      }

      /*
      (ast::Expr::DoubleQuotedString(d), _) => {
        h.highlight(d.syntax().text_range(), HighlightKind::String);
      }
      (ast::Expr::BlockExpr(b), _) => {
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
          if let Some(guard) = case.case_guard() {
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
      */
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

// FIXME: Dedupe from the HIR typer.
fn def_for_ty(db: &dyn HirDatabase, target: TargetId, ty: Type) -> Option<GlobalDefinition> {
  // TODO: Remove all this copy-pasta between here and HIR.
  let key = match ty {
    Type::Object(ref path) => DefinitionKey::Object(path.clone()),
    Type::Instance(ref path) => DefinitionKey::Instance(path.clone()),
    _ => return None,
  };

  warn!("key is {key:?}");

  for target in db.workspace().all_dependencies(target) {
    let defs = db.definitions_for_target(target);

    if let Some(def) = defs.items.get(&key) {
      return Some(def.clone());
    }
  }

  None
}

fn select_name_from_def(db: &dyn HirDatabase, def: GlobalDefinition, name: &str) -> Option<Type> {
  let block = match def.kind {
    GlobalDefinitionKind::Class(Some(_), _) => {
      BlockId::Class(AstId::new(def.ast_id)).in_file(def.file_id)
    }
    GlobalDefinitionKind::Trait(Some(_)) => {
      BlockId::Trait(AstId::new(def.ast_id)).in_file(def.file_id)
    }
    GlobalDefinitionKind::Object(Some(_)) => {
      BlockId::Object(AstId::new(def.ast_id)).in_file(def.file_id)
    }
    _ => return None,
  };

  let hir_ast = db.hir_ast_for_block(block);
  let inferred = db.infer(block, None);

  // Find all the `def` and `val`s in the block.
  let decls: Vec<_> = hir_ast
    .items
    .iter()
    .filter_map(|&it| match hir_ast.stmts[it] {
      hir::Stmt::Binding(ref b) => match b.kind {
        BindingKind::Def(_) | BindingKind::Val => {
          if b.name == *name {
            Some(it)
          } else {
            None
          }
        }
        _ => None,
      },
      _ => None,
    })
    .collect();

  match decls[..] {
    [] => None,
    [stmt_id] => inferred.stmts.get(&stmt_id).cloned(),
    _ => {
      // TODO: Resolve overloads.
      let stmt_id = decls.first().unwrap();

      inferred.stmts.get(&stmt_id).cloned()
    }
  }
}
