use std::{
  net::SocketAddr,
  os::unix::fs::FileTypeExt,
  path::{Path, PathBuf},
};

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

  #[error("failed to connect to BSP server after {0} tries")]
  MaxRetriesExceeded(u32),
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

  // Sockets are nice, but they only exist on linux. Solution: use linux!
  #[cfg(target_os = "linux")]
  Socket(PathBuf),
}

/// Which BSP server are we connected to?
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BspFlavor {
  Sbt,
  Bloop,
}

pub fn connect(dir: &Path, flavor: BspFlavor) -> Result<client::BspClient, BspError> {
  let config = match flavor {
    BspFlavor::Sbt => sbt_config(dir)?,
    BspFlavor::Bloop => {
      // TODO: Config to pick tcp/socket.
      // let config = bloop_socket_config(dir);

      let port = choose_port();
      bloop_tcp_config(port)
    }
  };

  client::BspClient::new(config)
}

#[allow(unused)]
fn choose_port() -> u16 {
  // This is kinda dumb, but it works well.
  let listener = std::net::TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], 0))).unwrap();
  listener.local_addr().unwrap().port()
}

#[allow(unused)]
fn bloop_socket_config(dir: &Path) -> BspConfig {
  restart_bloop();

  // I'm not making _another_ dot directory in each project. I'll just reuse the
  // `.bloop` directory.
  let socket_path = dir.join(".bloop").join("socket");

  if socket_path.exists() {
    let kind = socket_path.metadata().unwrap().file_type();

    // Remove the socket if it already exists. This assumes that there is only one
    // editor open per project.
    //
    // Detecting if there's already a BSP server connected to this socket is
    // difficult, so this ends up being a reliable solution.
    if kind.is_socket() {
      std::fs::remove_file(&socket_path).unwrap();
    } else {
      panic!("found a non-socket file at the socket path: {:?}", socket_path);
    }
  }

  BspConfig {
    command:  "/home/macmv/.local/share/coursier/bin/bloop".to_string(),
    argv:     vec![
      "bsp".to_string(),
      "--protocol".to_string(),
      "local".to_string(),
      "--socket".to_string(),
      socket_path.to_string_lossy().to_string(),
    ],
    protocol: BspProtocol::Socket(socket_path),
  }
}

#[allow(unused)]
fn bloop_tcp_config(port: u16) -> BspConfig {
  restart_bloop();

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

fn restart_bloop() {
  // Bloop gets busted if you kill a BSP server without closing it correctly. I've
  // already tried to close it correctly, but it still gets in a screwy state.
  // So I'll just restart the bloop server every time.
  //
  // TODO: Close the BSP server correctly.
  // TODO: Disable this hack for production builds.
  std::process::Command::new("/home/macmv/.local/share/coursier/bin/bloop")
    .arg("exit")
    .output()
    .expect("failed to create socket directory");
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
