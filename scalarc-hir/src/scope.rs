use scalarc_syntax::{
  ast::{AstNode, SyntaxKind},
  node::{NodeOrToken, SyntaxNode, SyntaxToken},
};

pub enum Declaration {
  Class,
  Val,
  Var,
  Def,
}

pub struct Scope {
  pub declarations: Vec<(String, Declaration)>,
}

// Returns the scopes around the given token. The first scope is the innermost.
pub fn scopes_for(token: &SyntaxToken) -> Vec<Scope> {
  let mut scopes = vec![];

  let mut n: NodeOrToken = token.clone().into();

  loop {
    scopes.push(collect_scope(&n));

    if let Some(parent) = n.parent() {
      n = parent.into();
    } else {
      break;
    }
  }

  scopes
}

fn collect_scope(t: &NodeOrToken) -> Scope {
  let mut declarations = vec![];

  for n in iter_prev_siblings(t) {
    match n.kind() {
      SyntaxKind::VAL_DEF => {
        let n = scalarc_syntax::ast::ValDef::cast(n).unwrap();
        if let Some(id) = n.id_token() {
          declarations.push((id.text().into(), Declaration::Val));
        }
      }
      _ => {}
    }
  }

  declarations.reverse();

  Scope { declarations }
}

fn iter_prev_siblings(t: &NodeOrToken) -> impl Iterator<Item = SyntaxNode> {
  struct Iter {
    prev: NodeOrToken,
  }

  impl Iterator for Iter {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<SyntaxNode> {
      loop {
        let next = self.prev.prev_sibling_or_token()?;
        self.prev = next.clone();
        if let Some(node) = next.as_node() {
          return Some(node.clone());
        }
      }
    }
  }

  Iter { prev: t.clone() }
}
