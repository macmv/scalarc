use crate::global::GlobalState;
use std::error::Error;

use scalarc_bsp::types::{self as bsp_types, BspRequest};

pub fn handle_workspace_build_targets(
  state: &mut GlobalState,
  result: bsp_types::WorkspaceBuildTargetsResult,
) -> Result<(), Box<dyn Error>> {
  // debug!("got build targets: {:#?}", result);

  if let Some(c) = &state.bsp_client {
    state.bsp_targets = Some(result.clone());

    // Grab all the sources!
    let id = c.request(bsp_types::SourcesParams {
      targets: result.targets.iter().map(|t| t.id.clone()).collect(),
    });
    state.bsp_requests.insert(id, bsp_types::SourcesParams::METHOD);
  }

  Ok(())
}

pub fn handle_sources(
  state: &mut GlobalState,
  sources: bsp_types::SourcesResult,
) -> Result<(), Box<dyn Error>> {
  // debug!("got sources: {:#?}", sources);

  if let Some(targets) = state.bsp_targets.take() {
    let workspace =
      crate::search::workspace_from_sources(targets, sources, &mut state.files.write());
    state.analysis_host.set_workspace(workspace);
  }

  Ok(())
}
