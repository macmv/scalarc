use std::error::Error;

pub fn handle_completion(
  _params: lsp_types::CompletionParams,
) -> Result<Option<lsp_types::CompletionResponse>, Box<dyn Error>> {
  Ok(Some(lsp_types::CompletionResponse::Array(vec![lsp_types::CompletionItem {
    label: "Hello, World!".to_string(),
    ..Default::default()
  }])))
}
