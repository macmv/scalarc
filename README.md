# ScalaRC

A Scala language server written in rust. Very heavily inspired by rust-analyzer.

## Crates

### `scalarc-parser` and `scalarc-syntax`

The `scalarc-parser` crate parses source code into a green tree, which is then
consumed by `scalarc-syntax`, which provides a structured AST. `scalarc-syntax`
is consumed elsewhere as "the" AST, and `scalarc-parser` is considered an
implementation detail.

`scalarc-syntax` only works with source code. It has no concept of the workspace
structure or where each file is located.

### `scalarc-hir`

This crate provides a standalone, structured syntax tree. It is built from
`scalarc-syntax`, but doesn't store any spans to the actual source. This allows
the result to be cached, and it doesn't need to change if whitespace or
filenames change.

### `scalarc-typer`

This crate, using `scalarc-hir`, provides type inference over a workspace.

### `scalarc-bsp`

This crate integrates with the BSP protocol, where we act as a client to a build
server (typically SBT or bloop). This crate only implements the protocol, and
doesn't store any actual state.

### `scalarc-analysis`

This crate stores a `salsa` database, with the entire workspace being analyzed.
This generally uses `scalarc-syntax` and `scalarc-typer`, and provides an API
for completions and other LSP-like functions. This crate knows nothing of LSP,
and just provides the "perfect" API for Scala.

### `scalarc-lsp`

Using `scalarc-analysis`, this crate is a binary which implements a language
server.
