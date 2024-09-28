use crate::global::GlobalState;
use std::error::Error;

use scalarc_bsp::{types as bsp_types, BspFlavor};

pub fn handle_log_message(
  snap: &mut GlobalState,
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

  let level = match (snap.bsp_flavor, params.ty) {
    // SBT logs all compilation errors as BSP errors (which is insane), so we mark
    // all of that nonsense as debug.
    (BspFlavor::Sbt, _) => log::Level::Debug,
    (BspFlavor::Bloop, bsp_types::MessageType::Error) => log::Level::Warn,
    (BspFlavor::Bloop, _) => log::Level::Info,
  };

  match ty {
    Some(ty) => log!(level, "bsp: {ty}: {}", params.message),
    None => log!(level, "bsp: {}", params.message),
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
