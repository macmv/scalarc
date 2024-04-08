//! Handles global state and the main loop of the server.

use crossbeam_channel::{Receiver, Select, Sender};
use parking_lot::RwLock;
use scalarc_analysis::{Analysis, AnalysisHost};
use scalarc_bsp::{client::BspClient, types as bsp_types};
use std::{error::Error, path::PathBuf, sync::Arc};

use lsp_types::{notification::Notification, Url};

use crate::files::Files;

pub struct GlobalState {
  pub sender:    Sender<lsp_server::Message>,
  pub workspace: PathBuf,

  pub files: Arc<RwLock<Files>>,

  pub analysis_host: AnalysisHost,
  pub bsp_client:    Option<BspClient>,

  // Temporary state between BSP requests.
  pub bsp_targets: Option<bsp_types::WorkspaceBuildTargetsResult>,

  response_sender:   Sender<lsp_server::Message>,
  response_receiver: Receiver<lsp_server::Message>,
}

pub(crate) struct GlobalStateSnapshot {
  pub analysis:  Analysis,
  pub files:     Arc<RwLock<Files>>,
  pub workspace: PathBuf,
}

enum Event {
  Message(lsp_server::Message),
  Response(lsp_server::Message),
  BspMessage(lsp_server::Message),
}

impl GlobalState {
  pub fn new(
    sender: Sender<lsp_server::Message>,
    bsp_client: Option<BspClient>,
    workspace: Url,
  ) -> Self {
    let (tx, rx) = crossbeam_channel::bounded(0);

    GlobalState {
      sender,
      workspace: workspace.to_file_path().unwrap(),
      files: Arc::new(RwLock::new(Files::new(workspace.to_file_path().unwrap()))),
      analysis_host: AnalysisHost::new(),
      bsp_client,
      bsp_targets: None,

      response_sender: tx,
      response_receiver: rx,
    }
  }

  pub fn run(
    mut self,
    receiver: Receiver<lsp_server::Message>,
    bsp_receiver: Option<Receiver<lsp_server::Message>>,
  ) -> Result<(), Box<dyn Error>> {
    while let Some(e) = self.next_event(&receiver, bsp_receiver.as_ref()) {
      match e {
        Event::Message(lsp_server::Message::Notification(lsp_server::Notification {
          method,
          ..
        }))
          if method == lsp_types::notification::Exit::METHOD =>
        {
          info!("shutting down due to exit notification");
          return Ok(());
        }

        _ => self.handle_event(e)?,
      }
    }

    error!("shutting down, client failed to send shutdown request");

    Ok(())
  }

  fn next_event(
    &self,
    receiver: &Receiver<lsp_server::Message>,
    bsp_receiver: Option<&Receiver<lsp_server::Message>>,
  ) -> Option<Event> {
    let mut sel = Select::new();
    sel.recv(receiver);
    sel.recv(&self.response_receiver);
    if let Some(r) = bsp_receiver {
      sel.recv(r);
    }

    let op = sel.select();

    match op.index() {
      0 => Some(Event::Message(op.recv(receiver).unwrap())),
      1 => Some(Event::Response(op.recv(&self.response_receiver).unwrap())),
      2 => Some(Event::BspMessage(op.recv(bsp_receiver.unwrap()).unwrap())),
      _ => None,
    }
  }

  fn handle_event(&mut self, e: Event) -> Result<(), Box<dyn Error>> {
    match e {
      Event::Message(lsp_server::Message::Request(req)) => self.handle_request(req),
      Event::Message(lsp_server::Message::Notification(not)) => self.handle_notification(not),
      Event::Message(lsp_server::Message::Response(_)) => (),
      Event::Response(e) => {
        self.sender.send(e)?;
      }
      Event::BspMessage(lsp_server::Message::Request(_)) => {}
      Event::BspMessage(lsp_server::Message::Response(res)) => self.handle_bsp_response(res),
      Event::BspMessage(lsp_server::Message::Notification(not)) => {
        self.handle_bsp_notification(not)
      }
    }

    self.process_changes();

    Ok(())
  }

  fn process_changes(&mut self) {
    let mut files = self.files.write();
    let changes = files.take_changes();

    for &file_id in &changes {
      self
        .analysis_host
        .change(scalarc_analysis::Change { file: file_id, text: files.read(file_id) });

      // This is re-computed on each change, because it makes the workspace structure
      // a lot simpler. Instead of the workspace storing all the files within the
      // source, we can just store a list of source roots, and then compute which
      // source root each file is in when its created.
      if !self.analysis_host.has_file(file_id) {
        let path = files.id_to_path(file_id);
        if let Some(source) = self
          .analysis_host
          .workspace()
          .sources
          .iter()
          .filter_map(|(id, s)| {
            let s = files.canonicalize(s)?;
            if path.starts_with(s) {
              Some(id)
            } else {
              None
            }
          })
          .next()
        {
          self.analysis_host.add_file(file_id, source);
          info!("new file at {} has id {file_id:?} and source {source:?}", path.display());
        }
      }
    }

    let snap = self.analysis_host.snapshot();

    for &file_id in &changes {
      let src = files.read(file_id);
      let diagnostics = snap.diagnostics(file_id).unwrap();

      self
        .sender
        .send(lsp_server::Message::Notification(lsp_server::Notification {
          method: lsp_types::notification::PublishDiagnostics::METHOD.into(),
          params: serde_json::to_value(lsp_types::PublishDiagnosticsParams {
            uri:         Url::from_file_path(self.workspace.join(files.id_to_path(file_id)))
              .unwrap(),
            diagnostics: diagnostics
              .into_iter()
              .map(|d| lsp_types::Diagnostic {
                message: d.message,
                range: lsp_types::Range {
                  start: pos_to_lsp(&src, d.span.start),
                  end:   pos_to_lsp(&src, d.span.end + 1), // TODO: Don't make empty spans
                },
                ..Default::default()
              })
              .collect(),
            version:     None,
          })
          .unwrap(),
        }))
        .unwrap();
    }
  }

  fn handle_request(&mut self, req: lsp_server::Request) {
    let mut dispatcher = RequestDispatcher { global: self, req };
    use crate::handler::request;
    use lsp_types::request as lsp_request;

    dispatcher
      // Not sure if we really need to do anything about a shutdown.
      .on_sync::<lsp_request::Shutdown>(|_, ()| Ok(()))
      .on::<lsp_request::SemanticTokensFullRequest>(request::handle_semantic_tokens_full)
      .on::<lsp_request::Completion>(request::handle_completion);
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

  fn handle_bsp_response(&mut self, res: lsp_server::Response) {
    let mut dispatcher = BspResponseDispatcher { global: self, res };

    use crate::handler::bsp_response;

    dispatcher
      .on_sync_mut::<bsp_types::WorkspaceBuildTargetsRequest>(
        bsp_response::handle_workspace_build_targets,
      )
      .on_sync_mut::<bsp_types::SourcesParams>(bsp_response::handle_sources);
  }

  fn handle_bsp_notification(&mut self, not: lsp_server::Notification) {
    let mut dispatcher = BspNotificationDispatcher { global: self, not };

    use crate::handler::bsp_notification;

    dispatcher.on_sync::<bsp_types::LogMessageParams>(bsp_notification::handle_log_message);
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
      files:     self.files.clone(),
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
  fn on_sync<R>(
    &mut self,
    f: fn(&GlobalState, R::Params) -> Result<R::Result, Box<dyn Error>>,
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

    // TODO: Dispatch this to a thread pool.
    let id = self.req.id.clone();
    let response = f(self.global, params).unwrap();
    self
      .global
      .sender
      .send(lsp_server::Message::Response(lsp_server::Response {
        id,
        result: Some(serde_json::to_value(response).unwrap()),
        error: None,
      }))
      .unwrap();

    self
  }

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

struct BspResponseDispatcher<'a> {
  global: &'a mut GlobalState,
  res:    lsp_server::Response,
}

impl BspResponseDispatcher<'_> {
  fn on_sync_mut<R>(
    &mut self,
    f: fn(&mut GlobalState, R::Result) -> Result<(), Box<dyn Error>>,
  ) -> &mut Self
  where
    R: scalarc_bsp::types::BspRequest,
  {
    // TODO
    // if self.res.method != R::METHOD {
    //   return self;
    // }

    let result = match serde_json::from_value::<R::Result>(self.res.result.clone().unwrap()) {
      Ok(p) => p,
      Err(e) => {
        error!("failed to deserialize params: {}", e);
        return self;
      }
    };

    // TODO: Dispatch this to a thread pool.
    let id = self.res.id.clone();
    let response = f(self.global, result).unwrap();
    self
      .global
      .sender
      .send(lsp_server::Message::Response(lsp_server::Response {
        id,
        result: Some(serde_json::to_value(response).unwrap()),
        error: None,
      }))
      .unwrap();

    self
  }
}

struct BspNotificationDispatcher<'a> {
  global: &'a mut GlobalState,
  not:    lsp_server::Notification,
}

impl BspNotificationDispatcher<'_> {
  fn on_sync<N>(&mut self, f: fn(&mut GlobalState, N) -> Result<(), Box<dyn Error>>) -> &mut Self
  where
    N: scalarc_bsp::types::BspNotification,
  {
    if self.not.method != N::METHOD {
      return self;
    }

    let params = match serde_json::from_value::<N>(self.not.params.clone()) {
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

// TODO: Store this in salsa.
fn pos_to_lsp(file: &str, pos: u32) -> lsp_types::Position {
  let mut offset = 0;

  for (i, line) in file.lines().enumerate() {
    if offset + line.len() as u32 >= pos {
      return lsp_types::Position { line: i as u32, character: pos - offset };
    }
    offset += line.len() as u32 + 1;
  }

  panic!("pos not in file");
}
