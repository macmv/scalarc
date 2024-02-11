use std::{error::Error, fs, path::PathBuf};

mod files;
mod global;
mod handler;
mod info;
mod search;

#[macro_use]
extern crate log;

fn main() {
  match run() {
    Ok(()) => (),
    Err(e) => {
      error!("{}", e);
      std::process::exit(1);
    }
  }
}

fn run() -> Result<(), Box<dyn Error>> {
  setup_logging();

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

  info!("starting LSP server in project root: {:?}", root_uri);

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

  let root_uri = root_uri.unwrap();

  let root_file = root_uri.to_file_path().unwrap();

  let bsp_client = match scalarc_bsp::connect(&root_file) {
    Ok(c) => {
      let res = c.send_initialize(root_uri.clone())?;
      let res: scalarc_bsp::types::InitializeBuildResult = serde_json::from_value(res).unwrap();

      info!("got initialize response: {:#?}", res);

      c.request(scalarc_bsp::types::WorkspaceBuildTargetsRequest);

      Some(c)
    }
    Err(e) => {
      error!("failed to connect to BSP server: {}", e);
      None
    }
  };

  // Keep `bsp_client` around to hold onto the reader/writer threads.
  //
  // TODO: Close the bsp server on an exit request.
  let bsp_receiver = bsp_client.as_ref().map(|c| c.receiver.clone());

  let global = global::GlobalState::new(connection.sender, bsp_client, root_uri);
  global.run(connection.receiver, bsp_receiver)?;

  Ok(())
}

fn setup_logging() {
  let dir = PathBuf::from("/home/macmv/.cache/scalarc");
  fs::create_dir_all(&dir).unwrap();

  simple_logging::log_to_file(dir.join("scalarc-lsp.log"), log::LevelFilter::Info).unwrap();

  // Copied the stdlibs panic hook, but uses `error!()` instead of stdout.
  std::panic::set_hook(Box::new(|info| {
    let location = info.location().unwrap_or_else(|| std::panic::Location::caller());

    let msg = match info.payload().downcast_ref::<&'static str>() {
      Some(s) => *s,
      None => match info.payload().downcast_ref::<String>() {
        Some(s) => &s[..],
        None => "Box<dyn Any>",
      },
    };

    let thread = std::thread::current();
    let name = thread.name().unwrap_or("<unnamed>");

    error!("thread '{name}' panicked at {location}:\n{msg}");
  }));
}
