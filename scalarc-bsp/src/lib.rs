use std::{net::SocketAddr, path::Path};

use discovery::BspJsonConfig;
use thiserror::Error;

pub mod client;
mod discovery;
pub mod types;

#[macro_use]
extern crate log;

#[derive(Error, Debug)]
pub enum BspError {
  #[error("no bsp servers found in workspace")]
  NoBspServers,

  #[error("server type was not found in workspace")]
  NotFound,

  #[error("error initializing bsp server: {0}")]
  InitializeError(String),
}

#[derive(Debug)]
pub struct BspConfig {
  pub command: String,
  pub argv:    Vec<String>,

  pub protocol: BspProtocol,
}

#[derive(Debug, PartialEq)]
pub enum BspProtocol {
  Stdio,
  Tcp(SocketAddr),
}

pub fn connect(_dir: &Path) -> Result<client::BspClient, BspError> {
  // let config = sbt_config(dir)?;
  let config = bloop_config(5101);

  Ok(client::BspClient::new(config))
}

fn bloop_config(port: u16) -> BspConfig {
  BspConfig {
    command:  "/home/macmv/.local/share/coursier/bin/bloop".to_string(),
    argv:     vec![
      "bsp".to_string(),
      "--protocol".to_string(),
      "tcp".to_string(),
      "--port".to_string(),
      port.to_string(),
    ],
    protocol: BspProtocol::Tcp(SocketAddr::from(([127, 0, 0, 1], port))),
  }
}

#[allow(unused)]
fn sbt_config(dir: &Path) -> Result<BspConfig, BspError> {
  let configs = discovery::find_bsp_servers(dir);

  if configs.is_empty() {
    return Err(BspError::NoBspServers);
  }

  if let Some(config) = configs.into_iter().find(|c| c.name == "sbt") {
    Ok(BspConfig::from_json(config))
  } else {
    Err(BspError::NotFound)
  }
}

impl BspConfig {
  pub(crate) fn from_json(json: BspJsonConfig) -> BspConfig {
    let command = json.argv[0].clone();
    let argv = json.argv[1..].to_vec();

    // Assume stdio for `.bsp` files.
    let protocol = BspProtocol::Stdio;

    BspConfig { command, argv, protocol }
  }
}
