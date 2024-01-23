//! The actual scala parser.

use crate::{
  syntax_kind::{SyntaxKind::*, T},
  Parser,
};

mod expr;
mod item;
mod type_expr;

pub mod entry_point;
