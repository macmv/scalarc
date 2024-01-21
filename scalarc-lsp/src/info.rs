pub fn version() -> &'static str { env!("CARGO_PKG_VERSION") }

pub fn server_capabilities() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    completion_provider: Some(lsp_types::CompletionOptions { ..Default::default() }),
    text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
      lsp_types::TextDocumentSyncKind::INCREMENTAL,
    )),
    ..Default::default()
  }
}
