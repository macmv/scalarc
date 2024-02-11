use crate::global::GlobalState;
use std::error::Error;

use scalarc_bsp::types as bsp_types;

pub fn handle_workspace_build_targets(
  state: &mut GlobalState,
  result: bsp_types::WorkspaceBuildTargetsResult,
) -> Result<(), Box<dyn Error>> {
  info!("got build targets: {:#?}", result);

  if let Some(c) = &state.bsp_client {
    state.bsp_targets = Some(result.clone());

    // Grab all the sources!
    c.request(bsp_types::SourcesParams {
      targets: result.targets.iter().map(|t| t.id.clone()).collect(),
    });
  }

  Ok(())
}

pub fn handle_sources(
  state: &mut GlobalState,
  sources: bsp_types::SourcesResult,
) -> Result<(), Box<dyn Error>> {
  info!("got sources: {:#?}", sources);

  if let Some(targets) = state.bsp_targets.take() {
    let workspace = crate::search::workspace_from_sources(targets, sources);
    state.analysis_host.set_workspace(workspace);
  }

  Ok(())
}
