use crate::global::GlobalState;
use std::error::Error;

use scalarc_bsp::types as bsp_types;

pub fn handle_workspace_build_targets(
  snap: &GlobalState,
  result: bsp_types::WorkspaceBuildTargetsResult,
) -> Result<(), Box<dyn Error>> {
  info!("got build targets: {:?}", result);

  Ok(())
}
