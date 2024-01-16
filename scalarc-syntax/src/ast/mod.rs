mod generated;

use std::marker::PhantomData;

use crate::node::{SyntaxElementChildren, SyntaxNode, SyntaxNodeChildren, SyntaxToken};
use scalarc_parser::SyntaxKind;

pub use generated::{nodes::*, tokens::*};

/// The main trait to go from untyped `SyntaxNode`  to a typed ast. The
/// conversion itself has zero runtime cost: ast and syntax nodes have exactly
/// the same representation: a pointer to the tree root and a pointer to the
/// node itself.
pub trait AstNode {
  fn can_cast(kind: SyntaxKind) -> bool
  where
    Self: Sized;

  fn cast(syntax: SyntaxNode) -> Option<Self>
  where
    Self: Sized;

  fn syntax(&self) -> &SyntaxNode;
  fn clone_for_update(&self) -> Self
  where
    Self: Sized,
  {
    Self::cast(self.syntax().clone_for_update()).unwrap()
  }
  fn clone_subtree(&self) -> Self
  where
    Self: Sized,
  {
    Self::cast(self.syntax().clone_subtree()).unwrap()
  }
}

/// Like `AstNode`, but wraps tokens rather than interior nodes.
pub trait AstToken {
  fn can_cast(token: SyntaxKind) -> bool
  where
    Self: Sized;

  fn cast(syntax: SyntaxToken) -> Option<Self>
  where
    Self: Sized;

  fn syntax(&self) -> &SyntaxToken;

  fn text(&self) -> &str { self.syntax().text() }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
  inner: SyntaxNodeChildren,
  ph:    PhantomData<N>,
}

impl<N> AstChildren<N> {
  fn new(parent: &SyntaxNode) -> Self { AstChildren { inner: parent.children(), ph: PhantomData } }
}

impl<N: AstNode> Iterator for AstChildren<N> {
  type Item = N;
  fn next(&mut self) -> Option<N> { self.inner.find_map(N::cast) }
}

/// An iterator over `SyntaxToken` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstTokenChildren {
  inner: SyntaxElementChildren,
  kind:  SyntaxKind,
}

impl AstTokenChildren {
  fn new(parent: &SyntaxNode, kind: SyntaxKind) -> Self {
    AstTokenChildren { inner: parent.children_with_tokens(), kind }
  }
}

impl Iterator for AstTokenChildren {
  type Item = SyntaxToken;
  fn next(&mut self) -> Option<SyntaxToken> {
    loop {
      let it = self.inner.next()?;
      if it.kind() == self.kind {
        match it.into_token() {
          Some(t) => return Some(t),
          _ => {}
        }
      }
    }
  }
}

mod support {
  use super::{AstChildren, AstNode, AstTokenChildren, SyntaxKind, SyntaxNode, SyntaxToken};

  pub(super) fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
    parent.children().find_map(N::cast)
  }

  pub(super) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
    AstChildren::new(parent)
  }

  pub(super) fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    parent.children_with_tokens().filter_map(|it| it.into_token()).find(|it| it.kind() == kind)
  }

  pub(super) fn token_children(parent: &SyntaxNode, kind: SyntaxKind) -> AstTokenChildren {
    AstTokenChildren::new(parent, kind)
  }
}
