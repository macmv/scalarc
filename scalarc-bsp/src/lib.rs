use std::path::Path;

use thiserror::Error;

mod client;
mod discovery;
pub mod types;

pub(crate) use discovery::BspConfig;

#[macro_use]
extern crate log;

#[derive(Error, Debug)]
pub enum BspError {
  #[error("no bsp servers found in workspace")]
  NoBspServers,

  #[error("no sbt server found in workspace")]
  NoSbtServer,

  #[error("error initializing bsp server: {0}")]
  InitializeError(String),
}

pub fn connect(dir: &Path) -> Result<client::BspClient, BspError> {
  let configs = discovery::find_bsp_servers(dir);

  if configs.is_empty() {
    return Err(BspError::NoBspServers);
  }

  for config in configs {
    // Prefer sbt for now. TODO: Might want to make this configurable.
    if config.name == "sbt" {
      return Ok(client::BspClient::new(config));
    }
  }

  Err(BspError::NoSbtServer)
}
