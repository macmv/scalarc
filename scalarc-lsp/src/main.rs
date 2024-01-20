use std::error::Error;

mod info;

fn main() {
  match run() {
    Ok(()) => (),
    Err(e) => {
      eprintln!("{}", e);
      std::process::exit(1);
    }
  }
}

fn run() -> Result<(), Box<dyn Error>> {
  // TODO: Use an epoll loop instead of spawning all these threads.
  let (connection, io_threads) = lsp_server::Connection::stdio();

  let (initialize_id, initialize_params) = match connection.initialize_start() {
    Ok(it) => it,
    Err(e) => {
      if e.channel_is_disconnected() {
        io_threads.join()?;
      }
      return Err(e.into());
    }
  };
  let lsp_types::InitializeParams { root_uri, .. } =
    serde_json::from_value::<lsp_types::InitializeParams>(initialize_params)?;

  println!("starting LSP server in project root: {:?}", root_uri);

  let server_capabilities = info::server_capabilities();

  let initialize_result = lsp_types::InitializeResult {
    capabilities: server_capabilities,
    server_info:  Some(lsp_types::ServerInfo {
      name:    String::from("scalarc"),
      version: Some(info::version().to_string()),
    }),
  };

  let initialize_result = serde_json::to_value(initialize_result).unwrap();

  if let Err(e) = connection.initialize_finish(initialize_id, initialize_result) {
    if e.channel_is_disconnected() {
      io_threads.join()?;
    }
    return Err(e.into());
  }

  println!("Hello, world!");

  Ok(())
}
