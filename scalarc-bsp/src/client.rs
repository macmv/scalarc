use std::{
  io,
  io::BufReader,
  process::Command,
  sync::atomic::{AtomicI32, Ordering},
  thread,
};

use crate::{
  types::{BspNotification, BspRequest},
  BspError,
};
use crossbeam_channel::{Receiver, Sender};
use lsp_server::{Message, RequestId};

pub struct BspClient {
  id: AtomicI32,

  pub sender:   Sender<lsp_server::Message>,
  pub receiver: Receiver<lsp_server::Message>,

  #[allow(unused)]
  threads: IoThreads,
}

impl BspClient {
  pub fn new(config: crate::BspConfig) -> Self {
    info!("creating BSP client with config: {:?}", config);

    assert_eq!(config.protocol, crate::BspProtocol::Stdio);

    let proc = Command::new(&config.command)
      .args(&config.argv)
      .stdout(std::process::Stdio::piped())
      .stderr(std::process::Stdio::piped())
      .stdin(std::process::Stdio::piped())
      .spawn()
      .expect("failed to start BSP server");

    let mut child_stdin = proc.stdin.unwrap();
    let mut child_stdout = BufReader::new(proc.stdout.unwrap());

    let (writer_sender, writer_receiver) = crossbeam_channel::bounded::<Message>(0);
    let writer = thread::spawn(move || {
      for msg in writer_receiver {
        match msg.write(&mut child_stdin) {
          Ok(_) => {}
          Err(e) => {
            error!("failed to write message: {}", e);
            break;
          }
        }
      }
      error!("closing writer thread");
      Ok(())
    });
    let (reader_sender, reader_receiver) = crossbeam_channel::bounded::<Message>(0);
    let reader = thread::spawn(move || loop {
      match Message::read(&mut child_stdout) {
        Ok(Some(msg)) => {
          let is_exit = match &msg {
            Message::Notification(n) => n.method == "exit",
            _ => false,
          };

          reader_sender.send(msg).unwrap();

          if is_exit {
            break Ok(());
          }
        }
        Ok(None) => {
          error!("closing reader thread");
          break Ok(());
        }
        Err(e) => {
          error!("failed to read message: {}", e);
          break Err(e);
        }
      }
    });
    let threads = IoThreads { reader, writer };

    BspClient { id: AtomicI32::new(1), sender: writer_sender, receiver: reader_receiver, threads }
  }

  pub fn request<R: BspRequest>(&self, params: R) -> RequestId {
    let id: RequestId = self.id.fetch_add(1, Ordering::SeqCst).into();

    self
      .sender
      .send(Message::Request(lsp_server::Request {
        id:     id.clone(),
        method: R::METHOD.into(),
        params: serde_json::to_value(params).unwrap(),
      }))
      .unwrap();

    id
  }

  pub fn notify<N: BspNotification>(&self, notification: N) {
    self
      .sender
      .send(Message::Notification(lsp_server::Notification {
        method: N::METHOD.into(),
        params: serde_json::to_value(notification).unwrap(),
      }))
      .unwrap();
  }

  // TODO: Pass in capabilities.
  pub fn send_initialize(&self, root_uri: lsp_types::Url) -> Result<serde_json::Value, BspError> {
    let id = self.request(crate::types::InitializeBuildParams {
      display_name: "scalarc".to_string(),
      version: "1.0.0".to_string(),
      root_uri: Some(root_uri),
      ..Default::default()
    });

    loop {
      match self.receiver.recv() {
        Ok(Message::Response(res)) if res.id == id => {
          self
            .sender
            .send(Message::Notification(lsp_server::Notification {
              method: "build/initialized".to_string(),
              params: serde_json::json!(null),
            }))
            .unwrap();

          return Ok(res.result.unwrap());
        }
        Ok(msg) => {
          return Err(BspError::InitializeError(format!(
            "expected initialize response, got {:?}",
            msg
          )))
        }
        Err(e) => {
          return Err(BspError::InitializeError(format!(
            "expected initialize response, got error: {}",
            e
          )))
        }
      }
    }
  }
}

#[allow(unused)]
pub struct IoThreads {
  reader: thread::JoinHandle<io::Result<()>>,
  writer: thread::JoinHandle<io::Result<()>>,
}
