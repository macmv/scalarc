use std::{io::Read, process::Command, thread::JoinHandle};

pub struct BspClient {}

pub struct BspConnection {
  thread: JoinHandle<()>,
}

impl BspClient {
  pub fn new(config: crate::BspConfig) -> Self {
    info!("creating BSP client with config: {:?}", config);

    let proc = Command::new(&config.argv[0])
      .args(&config.argv[1..])
      .stdout(std::process::Stdio::piped())
      .stderr(std::process::Stdio::piped())
      .stdin(std::process::Stdio::piped())
      .spawn()
      .expect("failed to start BSP server");

    BspClient {}
  }
}

impl BspConnection {
  pub fn spawn(config: crate::BspConfig) -> BspConnection {
    let thread = std::thread::spawn(move || {
      let _client = BspClient::new(config);
    });

    BspConnection { thread }
  }
}
