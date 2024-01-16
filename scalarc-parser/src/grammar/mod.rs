//! The actual scala parser.

use crate::{
  syntax_kind::{SyntaxKind::*, T},
  Marker, Parser,
};

mod items;

pub mod entry_point;
