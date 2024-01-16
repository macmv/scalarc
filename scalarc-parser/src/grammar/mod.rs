//! The actual scala parser.

use crate::{
  syntax_kind::{SyntaxKind::*, T},
  Marker, Parser,
};

mod expr;
mod item;

pub mod entry_point;
