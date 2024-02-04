use std::{collections::HashMap, mem, sync::Arc};

use la_arena::Idx;
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{AstNode, SyntaxKind},
  node::SyntaxNode,
  SyntaxNodePtr, TextSize,
};

use crate::HirDatabase;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxId<T> {
  raw:      Idx<scalarc_syntax::SyntaxNodePtr>,
  _phantom: std::marker::PhantomData<fn() -> T>,
}

/// Maps `SyntaxNode`s to `SyntaxId`s.
#[derive(Default, Debug)]
pub struct SourceMap {
  arena: la_arena::Arena<SyntaxNodePtr>,
  items: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}

impl PartialEq for SourceMap {
  fn eq(&self, other: &Self) -> bool { self.arena == other.arena }
}
impl Eq for SourceMap {}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct SpanMap {
  file:  FileId,
  pairs: Box<[(TextSize, SyntaxNodePtr)]>,
  end:   TextSize,
}

impl<T> SyntaxId<T> {
  pub fn get(&self, db: &dyn HirDatabase, file: FileId) -> SyntaxNodePtr {
    db.source_map(file).arena[self.raw]
  }
}

pub fn source_map(db: &dyn HirDatabase, file: FileId) -> Arc<SourceMap> {
  let source = db.parse(file);
  let map = SourceMap::from_source(source.syntax_node());
  Arc::new(map)
}

pub fn span_map(db: &dyn HirDatabase, file: FileId) -> Arc<SpanMap> {
  let source = db.source_map(file);
  let ast = db.parse(file);
  let pairs = ast
    .tree()
    .items()
    .map(|item| (item.syntax().text_range().start(), source.id(&item).unwrap().get(db, file)))
    .collect::<Vec<_>>();

  Arc::new(SpanMap {
    file,
    pairs: pairs.into_boxed_slice(),
    end: ast.syntax_node().text_range().end(),
  })
}

impl SourceMap {
  fn from_source(source: SyntaxNode) -> Self {
    let mut map = SourceMap::default();

    fn should_alloc(node: &SyntaxNode) -> bool {
      match node.kind() {
        SyntaxKind::VAL_DEF => true,
        SyntaxKind::FUN_DEF => true,
        _ => false,
      }
    }

    map.alloc(&source);

    let mut current = vec![source];
    let mut other = vec![];

    while !current.is_empty() {
      for item in current.drain(..) {
        let mut preorder = item.preorder();
        while let Some(event) = preorder.next() {
          match event {
            scalarc_syntax::WalkEvent::Enter(node) => {
              if should_alloc(&node) {
                map.alloc(&node);
                other.extend(node.children());
                preorder.skip_subtree();
              }
            }
            scalarc_syntax::WalkEvent::Leave(_) => {}
          }
        }
      }
      mem::swap(&mut current, &mut other);
    }

    map
  }

  pub fn id<T: AstNode>(&self, item: &T) -> Option<SyntaxId<T>> {
    let ptr = SyntaxNodePtr::new(item.syntax());
    match self.items.get(&ptr) {
      Some(id) => Some(SyntaxId { raw: *id, _phantom: std::marker::PhantomData }),
      None => None,
    }
  }

  fn alloc(&mut self, item: &SyntaxNode) {
    let ptr = SyntaxNodePtr::new(&item);
    let id = self.arena.alloc(ptr);
    self.items.insert(ptr, id);
  }
}
