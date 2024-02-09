use crate::global::GlobalState;
use std::error::Error;

use scalarc_bsp::types as bsp_types;

pub fn handle_workspace_build_targets(
  state: &GlobalState,
  result: bsp_types::WorkspaceBuildTargetsResult,
) -> Result<(), Box<dyn Error>> {
  info!("got build targets: {:#?}", result);

  if let Some(c) = &state.bsp_client {
    // Grab all the sources!
    c.request(bsp_types::SourcesParams {
      targets: result.targets.iter().map(|t| t.id.clone()).collect(),
    });
  }

  Ok(())
}

pub fn handle_sources(
  state: &GlobalState,
  result: bsp_types::SourcesResult,
) -> Result<(), Box<dyn Error>> {
  info!("got sources: {:#?}", result);

  Ok(())
}
