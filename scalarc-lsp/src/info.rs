pub fn version() -> &'static str { env!("CARGO_PKG_VERSION") }

pub fn server_capabilities() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    completion_provider: Some(lsp_types::CompletionOptions { ..Default::default() }),
    text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
      lsp_types::TextDocumentSyncKind::INCREMENTAL,
    )),

    semantic_tokens_provider: Some(
      lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
        lsp_types::SemanticTokensOptions {
          legend: crate::handler::request::semantic_tokens_legend(),
          range: Some(true),
          full: Some(lsp_types::SemanticTokensFullOptions::Delta { delta: Some(true) }),
          ..Default::default()
        },
      ),
    ),

    definition_provider: Some(lsp_types::OneOf::Right(lsp_types::DefinitionOptions {
      work_done_progress_options: lsp_types::WorkDoneProgressOptions {
        // TODO: This would be neat to implement. Not sure how though.
        work_done_progress: Some(false),
      },
    })),

    document_highlight_provider: Some(lsp_types::OneOf::Left(true)),

    hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),

    ..Default::default()
  }
}
