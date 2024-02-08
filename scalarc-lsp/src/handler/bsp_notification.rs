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
