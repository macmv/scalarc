//! Handles global state and the main loop of the server.

use crossbeam_channel::{select, Receiver, Sender};
use scalarc_analysis::{Analysis, AnalysisHost};
use std::{error::Error, path::PathBuf};

use lsp_types::Url;

use crate::files::Files;

pub struct GlobalState {
  pub sender:    Sender<lsp_server::Message>,
  pub workspace: PathBuf,

  pub files: Files,

  pub analysis_host: AnalysisHost,

  response_sender:   Sender<lsp_server::Message>,
  response_receiver: Receiver<lsp_server::Message>,
}

pub(crate) struct GlobalStateSnapshot {
  pub analysis:  Analysis,
  pub workspace: PathBuf,
}

impl GlobalState {
  pub fn new(sender: Sender<lsp_server::Message>, workspace: Url) -> Self {
    let (tx, rx) = crossbeam_channel::bounded(0);

    GlobalState {
      sender,
      workspace: workspace.to_file_path().unwrap(),
      files: Files::new(),
      analysis_host: AnalysisHost::new(),

      response_sender: tx,
      response_receiver: rx,
    }
  }

  pub fn run(mut self, receiver: Receiver<lsp_server::Message>) -> Result<(), Box<dyn Error>> {
    // TODO: Spawn a task to fetch bsp status here.

    loop {
      select! {
        recv(receiver) -> msg => {
          if let Ok(msg) = msg {
            self.handle_event(msg);
          }
        },
        recv(self.response_receiver) -> msg => {
          if let Ok(msg) = msg {
            let _ = self.sender.send(msg);
          }
        }
      }
    }
  }

  fn handle_event(&mut self, ev: lsp_server::Message) {
    match ev {
      lsp_server::Message::Request(req) => self.handle_request(req),
      lsp_server::Message::Notification(not) => self.handle_notification(not),
      lsp_server::Message::Response(_) => (),
    }
  }

  fn handle_request(&mut self, req: lsp_server::Request) {
    let mut dispatcher = RequestDispatcher { global: self, req };

    use crate::handler::request;
    use lsp_types::request as lsp_request;

    dispatcher.on::<lsp_request::Completion>(request::handle_completion);
  }

  fn handle_notification(&mut self, not: lsp_server::Notification) {
    let mut dispatcher = NotificationDispatcher { global: self, not };

    use crate::handler::notification;
    use lsp_types::notification as lsp_notification;

    dispatcher
      .on_sync::<lsp_notification::DidOpenTextDocument>(notification::handle_open_text_document)
      .on_sync::<lsp_notification::DidChangeTextDocument>(
        notification::handle_change_text_document,
      );
  }

  pub fn workspace_path(&self, uri: &Url) -> Option<PathBuf> {
    if uri.scheme() != "file" {
      return None;
    }

    let path = uri.to_file_path().ok()?;
    path.strip_prefix(&self.workspace).ok().map(Into::into)
  }

  pub fn snapshot(&self) -> GlobalStateSnapshot {
    GlobalStateSnapshot {
      analysis:  self.analysis_host.snapshot(),
      workspace: self.workspace.clone(),
    }
  }
}

impl GlobalStateSnapshot {
  pub fn workspace_path(&self, uri: &Url) -> Option<PathBuf> {
    if uri.scheme() != "file" {
      return None;
    }

    let path = uri.to_file_path().ok()?;
    path.strip_prefix(&self.workspace).ok().map(Into::into)
  }
}

struct RequestDispatcher<'a> {
  global: &'a mut GlobalState,
  req:    lsp_server::Request,
}

impl RequestDispatcher<'_> {
  fn on<R>(
    &mut self,
    f: fn(GlobalStateSnapshot, R::Params) -> Result<R::Result, Box<dyn Error>>,
  ) -> &mut Self
  where
    R: lsp_types::request::Request,
  {
    if self.req.method != R::METHOD {
      return self;
    }

    let params = match serde_json::from_value::<R::Params>(self.req.params.clone()) {
      Ok(p) => p,
      Err(e) => {
        error!("failed to deserialize params: {}", e);
        return self;
      }
    };

    let snapshot = self.global.snapshot();

    // TODO: Dispatch this to a thread pool.
    let responder = self.global.response_sender.clone();
    let id = self.req.id.clone();
    std::thread::spawn(move || {
      let response = f(snapshot, params).unwrap();
      responder
        .send(lsp_server::Message::Response(lsp_server::Response {
          id,
          result: Some(serde_json::to_value(response).unwrap()),
          error: None,
        }))
        .unwrap();
    });

    self
  }
}

struct NotificationDispatcher<'a> {
  global: &'a mut GlobalState,
  not:    lsp_server::Notification,
}

impl NotificationDispatcher<'_> {
  fn on_sync<N>(
    &mut self,
    f: fn(&mut GlobalState, N::Params) -> Result<(), Box<dyn Error>>,
  ) -> &mut Self
  where
    N: lsp_types::notification::Notification,
  {
    if self.not.method != N::METHOD {
      return self;
    }

    let params = match serde_json::from_value::<N::Params>(self.not.params.clone()) {
      Ok(p) => p,
      Err(e) => {
        error!("failed to deserialize params: {}", e);
        return self;
      }
    };
    // TODO: Dispatch this to a thread pool.
    f(self.global, params).unwrap();

    self
  }
}
