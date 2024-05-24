use crate::global::GlobalState;
use std::error::Error;

use scalarc_bsp::types as bsp_types;

pub fn handle_log_message(
  _snap: &mut GlobalState,
  params: bsp_types::LogMessageParams,
) -> Result<(), Box<dyn Error>> {
  // BSP servers kinda do things, so errors and warnings will be too annoying. So
  // they get logged as info messages.
  let ty = match params.ty {
    bsp_types::MessageType::Error => Some("error"),
    bsp_types::MessageType::Warning => Some("warning"),
    bsp_types::MessageType::Info => Some("info"),
    bsp_types::MessageType::Log => None,
  };

  match ty {
    Some(ty) => info!("bsp: {ty}: {}", params.message),
    None => info!("bsp: {}", params.message),
  };

  Ok(())
}

pub fn handle_diagnostics(
  global: &mut GlobalState,
  params: bsp_types::PublishDiagnosticsParams,
) -> Result<(), Box<dyn Error>> {
  if let Some(path) = global.workspace_path(&params.text_document.uri) {
    let file_id = global.files.read().path_to_id(&path);

    let diagnostics = global.diagnostics.entry(file_id).or_default();

    let original_diagnostics = diagnostics.clone();

    if params.reset {
      diagnostics.clear();
    }
    diagnostics.extend(params.diagnostics);

    if original_diagnostics != *diagnostics {
      global.diagnostic_changes.push(file_id);
    }

    Ok(())
  } else {
    Ok(())
  }
}
