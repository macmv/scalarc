use std::path::Path;

use thiserror::Error;

mod client;
mod discovery;

pub(crate) use discovery::BspConfig;

#[macro_use]
extern crate log;

#[derive(Error, Debug)]
pub enum BspError {
  #[error("no bsp servers found in workspace")]
  NoBspServers,

  #[error("no sbt server found in workspace")]
  NoSbtServer,
}

pub fn connect(dir: &Path) -> Result<client::BspConnection, BspError> {
  let configs = discovery::find_bsp_servers(dir);

  if configs.is_empty() {
    return Err(BspError::NoBspServers);
  }

  for config in configs {
    // Prefer sbt for now. TODO: Might want to make this configurable.
    if config.name == "sbt" {
      return Ok(client::BspConnection::spawn(config));
    }
  }

  Err(BspError::NoSbtServer)
}
