//! Maps AST nodes to stable IDs, that can be used across reparses.

use std::{
  hash::{BuildHasher, BuildHasherDefault, DefaultHasher, Hash, Hasher},
  marker::PhantomData,
  panic::RefUnwindSafe,
  sync::Arc,
};

use la_arena::{Arena, Idx};
use scalarc_source::FileId;
use scalarc_syntax::{
  ast::{self, AstNode, SyntaxKind},
  node::SyntaxNode,
  Parse, SourceFile, SyntaxNodePtr,
};

use crate::HirDatabase;

mod expr;

pub use expr::*;

#[derive(Default, Debug)]
pub struct AstIdMap {
  arena: Arena<SyntaxNodePtr>,
  map:   hashbrown::HashMap<Idx<SyntaxNodePtr>, (), ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstId<N: AstItem> {
  raw: Idx<SyntaxNodePtr>,

  // fn() -> N so that `AstId` is still Send + Sync.
  phantom: PhantomData<fn() -> N>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ErasedAstId {
  pub(crate) raw: Idx<SyntaxNodePtr>,
}

impl<N: AstItem> AstId<N> {
  pub fn erased(&self) -> ErasedAstId { ErasedAstId { raw: self.raw } }

  // TODO: Make this harder to do.
  pub fn new(erased: ErasedAstId) -> Self { AstId { raw: erased.raw, phantom: PhantomData } }
}

impl PartialEq for AstIdMap {
  fn eq(&self, other: &Self) -> bool { self.arena == other.arena }
}
impl Eq for AstIdMap {}

pub(crate) fn item_id_map(db: &dyn HirDatabase, file_id: FileId) -> Arc<AstIdMap> {
  let node = db.parse(file_id);

  Arc::new(AstIdMap::from_source(&node.syntax_node()))
}

impl AstIdMap {
  pub(crate) fn from_source(node: &SyntaxNode) -> AstIdMap {
    assert!(node.parent().is_none());
    let mut res = AstIdMap::default();

    // make sure to allocate the root node
    if !should_alloc_id(node.kind()) {
      res.arena.alloc(SyntaxNodePtr::new(node));
    }
    // By walking the tree in breadth-first order we make sure that parents
    // get lower ids then children. That is, adding a new child does not
    // change parent's id. This means that, say, adding a new function to a
    // trait does not change ids of top-level items, which helps caching.
    bdfs(node, |it| {
      if should_alloc_id(it.kind()) {
        res.arena.alloc(SyntaxNodePtr::new(&it));
        TreeOrder::BreadthFirst
      } else {
        TreeOrder::DepthFirst
      }
    });
    res.map = hashbrown::HashMap::with_capacity_and_hasher(res.arena.len(), ());
    for (idx, ptr) in res.arena.iter() {
      let hash = hash_ptr(ptr);
      match res.map.raw_entry_mut().from_hash(hash, |idx2| *idx2 == idx) {
        hashbrown::hash_map::RawEntryMut::Occupied(_) => unreachable!(),
        hashbrown::hash_map::RawEntryMut::Vacant(entry) => {
          entry.insert_with_hasher(hash, idx, (), |&idx| hash_ptr(&res.arena[idx]));
        }
      }
    }
    res.arena.shrink_to_fit();
    res
  }

  pub fn iter(&self) -> impl Iterator<Item = (ErasedAstId, &SyntaxNodePtr)> {
    self.arena.iter().map(move |(id, ptr)| (ErasedAstId { raw: id }, ptr))
  }

  pub fn item_id<N: AstItem>(&self, item: &N) -> AstId<N> {
    let raw = self.erased_item_id(item.syntax()).raw;
    AstId { raw, phantom: PhantomData }
  }

  pub fn erased_item_id(&self, node: &SyntaxNode) -> ErasedAstId {
    let raw = self.erased_item_idx(node);
    ErasedAstId { raw }
  }

  pub fn contains_node(&self, node: &SyntaxNode) -> bool {
    let ptr = SyntaxNodePtr::new(node);
    let hash = hash_ptr(&ptr);
    self.map.raw_entry().from_hash(hash, |&idx| self.arena[idx] == ptr).is_some()
  }

  pub fn get_erased(&self, id: ErasedAstId) -> SyntaxNodePtr { self.arena[id.raw] }

  pub fn get<N: AstItem>(&self, ast: &Parse<SourceFile>, id: AstId<N>) -> N {
    let ptr = self.get_erased(id.erased());

    N::cast(ptr.to_node(ast.tree().syntax())).unwrap()
  }

  fn erased_item_idx(&self, item: &SyntaxNode) -> Idx<SyntaxNodePtr> {
    let ptr = SyntaxNodePtr::new(item);
    let hash = hash_ptr(&ptr);
    match self.map.raw_entry().from_hash(hash, |&idx| self.arena[idx] == ptr) {
      Some((&idx, &())) => idx,
      None => panic!(
        "can't find {:?} in ItemIdMap:\n{:?}",
        item,
        self.arena.iter().map(|(_id, i)| i).collect::<Vec<_>>(),
      ),
    }
  }
}

#[derive(PartialEq, Eq)]
enum TreeOrder {
  DepthFirst,
  BreadthFirst,
}

fn bdfs(node: &SyntaxNode, mut f: impl FnMut(SyntaxNode) -> TreeOrder) {
  let mut curr_layer = vec![node.clone()];
  let mut next_layer = vec![];
  while !curr_layer.is_empty() {
    curr_layer.drain(..).for_each(|node| {
      let mut preorder = node.preorder();
      while let Some(event) = preorder.next() {
        match event {
          scalarc_syntax::WalkEvent::Enter(node) => {
            if f(node.clone()) == TreeOrder::BreadthFirst {
              next_layer.extend(node.children());
              preorder.skip_subtree();
            }
          }
          scalarc_syntax::WalkEvent::Leave(_) => {}
        }
      }
    });
    std::mem::swap(&mut curr_layer, &mut next_layer);
  }
}

// "AST Items" are items that can be looked up in the global scope. This
// includes classes, functions, val definitions in class bodies, and more.
//
// Notably, this does not include variables local to function bodies.
pub trait AstItem: AstNode {}
macro_rules! register_ast_item {
  (impl AstItem for $($ident:ident),+ ) => {
    $(
      impl AstItem for ast::$ident {}
    )+
    fn should_alloc_id(kind: SyntaxKind) -> bool {
      $(
        ast::$ident::can_cast(kind)
      )||+
    }
  };
}
register_ast_item! {
  impl AstItem for
  Item,
    ClassDef,
    ObjectDef,
    FunDef,
      FunParam,
    ValDef,
  ItemBody, BlockExpr
}

fn hash_ptr(ptr: &SyntaxNodePtr) -> u64 {
  let mut hasher = BuildHasherDefault::<DefaultHasher>::default().build_hasher();
  ptr.hash(&mut hasher);
  hasher.finish()
}
