use std::error::Error;

pub fn handle_open_text_document(
  params: lsp_types::DidOpenTextDocumentParams,
) -> Result<(), Box<dyn Error>> {
  info!("opened text document {:?}", params.text_document.uri);

  Ok(())
}

pub fn handle_change_text_document(
  params: lsp_types::DidChangeTextDocumentParams,
) -> Result<(), Box<dyn Error>> {
  info!("changed text document {:?}", params.text_document.uri);

  Ok(())
}
