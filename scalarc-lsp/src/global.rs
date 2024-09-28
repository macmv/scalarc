//! Handles global state and the main loop of the server.

use crossbeam_channel::{Receiver, RecvError, Select, Sender};
use lsp_server::{ErrorCode, RequestId};
use parking_lot::RwLock;
use scalarc_analysis::{Analysis, AnalysisHost};
use scalarc_bsp::{client::BspClient, types as bsp_types};
use scalarc_source::{FileId, SourceRootId, Workspace};
use scalarc_syntax::TextSize;
use std::{collections::HashMap, error::Error, path::PathBuf, sync::Arc};

use lsp_types::{notification::Notification, Url};

use crate::files::Files;

pub struct GlobalState {
  pub sender:    Sender<lsp_server::Message>,
  pub workspace: PathBuf,

  pub files: Arc<RwLock<Files>>,

  /// Maps file IDs to source roots. If there is no source root for a file
  /// (happens during initialization usually, or for files outside a worksapce),
  /// then `None` will be set for it.
  pub file_to_source_root: HashMap<FileId, Option<SourceRootId>>,

  pub analysis_host: AnalysisHost,
  pub bsp_client:    Option<BspClient>,

  /// Diagnostics from the build server. These are not all the diagnostics seen
  /// from the client, as parsing errors from `scalarc-syntax` will be fetched
  /// of the analysis host instead.
  pub diagnostics:        HashMap<FileId, Vec<lsp_types::Diagnostic>>,
  pub diagnostic_changes: Vec<FileId>,

  // Temporary state between BSP requests.
  pub bsp_targets: Option<bsp_types::WorkspaceBuildTargetsResult>,

  // FIXME: This really shouldn't live here. Not sure where else to put it.
  //
  // Also it should timeout requests that failed to reply.
  pub bsp_requests: HashMap<RequestId, &'static str>,

  response_sender:   Sender<lsp_server::Message>,
  response_receiver: Receiver<lsp_server::Message>,

  pool_sender: Sender<Box<dyn FnOnce() + Send>>,
  #[allow(unused)]
  pool:        Vec<std::thread::JoinHandle<()>>,
}

pub(crate) struct GlobalStateSnapshot {
  pub analysis:  Analysis,
  pub files:     Arc<RwLock<Files>>,
  pub workspace: PathBuf,
}

#[derive(Debug)]
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
    let (pool_tx, pool_rx) = crossbeam_channel::bounded::<Box<dyn FnOnce() + Send>>(0);

    let pool = (0..16)
      .map(|_| {
        let rx = pool_rx.clone();
        std::thread::spawn(move || {
          while let Ok(f) = rx.recv() {
            f();
          }
        })
      })
      .collect();

    let (tx, rx) = crossbeam_channel::bounded(0);

    GlobalState {
      sender,
      workspace: workspace.to_file_path().unwrap(),

      files: Arc::new(RwLock::new(Files::new(workspace.to_file_path().unwrap()))),
      file_to_source_root: HashMap::new(),

      analysis_host: AnalysisHost::new(),
      bsp_client,

      diagnostics: HashMap::new(),
      diagnostic_changes: vec![],

      bsp_targets: None,
      bsp_requests: HashMap::new(),

      response_sender: tx,
      response_receiver: rx,

      pool_sender: pool_tx,
      pool,
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

          if let Some(c) = self.bsp_client {
            c.shutdown();
          }

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
      2 => match op.recv(bsp_receiver.unwrap()) {
        Ok(m) => Some(Event::BspMessage(m)),
        Err(RecvError) => {
          error!("BSP client disconnected, shutting down");
          None
        }
      },
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

  pub fn set_workspace(&mut self, workspace: Workspace) {
    for (id, root) in workspace.source_roots.iter() {
      for &file in &root.sources {
        self.file_to_source_root.insert(file, Some(id));
      }
    }

    self.analysis_host.set_workspace(workspace);
  }

  fn process_changes(&mut self) {
    let mut files = self.files.write();
    let changes = files.take_changes();

    for &file_id in &changes {
      if let std::collections::hash_map::Entry::Vacant(e) = self.file_to_source_root.entry(file_id)
      {
        e.insert(None);
        self.analysis_host.add_file(file_id);
      }

      self
        .analysis_host
        .change(scalarc_analysis::Change { file: file_id, text: files.read(file_id) });
    }

    let snap = self.analysis_host.snapshot();

    for file_id in changes.iter().copied().chain(self.diagnostic_changes.drain(..)) {
      let line_index = snap.line_index(file_id).unwrap();
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
              .filter_map(|d| {
                let start = line_index.try_line_col(d.span.start())?;

                let end = if d.span.is_empty() {
                  // Underline the next character for empty spans.
                  line_index.try_line_col(TextSize::from(u32::from(d.span.end()) + 1))?
                } else {
                  line_index.try_line_col(d.span.end())?
                };

                Some(lsp_types::Diagnostic {
                  message: d.message,
                  range: lsp_types::Range {
                    start: lsp_types::Position { line: start.line, character: start.col },
                    end:   lsp_types::Position { line: end.line, character: end.col },
                  },
                  ..Default::default()
                })
              })
              .chain(self.diagnostics.get(&file_id).unwrap_or(&vec![]).iter().cloned())
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
      .on::<lsp_request::GotoDefinition>(request::handle_goto_definition)
      .on::<lsp_request::DocumentHighlightRequest>(request::handle_document_highlight)
      .on::<lsp_request::HoverRequest>(request::handle_hover)
      .on::<lsp_request::Completion>(request::handle_completion);
  }

  fn handle_notification(&mut self, not: lsp_server::Notification) {
    let mut dispatcher = NotificationDispatcher { global: self, not };

    use crate::handler::notification;
    use lsp_types::notification as lsp_notification;

    dispatcher
      .on_sync::<lsp_notification::DidOpenTextDocument>(notification::handle_open_text_document)
      .on_sync::<lsp_notification::DidChangeTextDocument>(notification::handle_change_text_document)
      .on_sync::<lsp_notification::DidSaveTextDocument>(notification::handle_save_text_document);
  }

  fn handle_bsp_response(&mut self, res: lsp_server::Response) {
    let mut dispatcher = BspResponseDispatcher { global: self, res };

    use crate::handler::bsp_response;

    dispatcher
      .on_sync_mut::<bsp_types::WorkspaceBuildTargetsRequest>(
        bsp_response::handle_workspace_build_targets,
      )
      .on_sync_mut::<bsp_types::SourcesParams>(bsp_response::handle_sources)
      .on_sync_mut::<bsp_types::BuildTargetCompileRequest>(bsp_response::handle_compile_result);
  }

  fn handle_bsp_notification(&mut self, not: lsp_server::Notification) {
    let mut dispatcher = BspNotificationDispatcher { global: self, not };

    use crate::handler::bsp_notification;

    dispatcher
      .on_sync::<bsp_types::LogMessageParams>(bsp_notification::handle_log_message)
      .on_sync::<bsp_types::PublishDiagnosticsParams>(bsp_notification::handle_diagnostics);
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
  fn log_error<R>(&self, e: impl Error)
  where
    R: lsp_types::request::Request,
  {
    error!(
      "in request dispatcher for {}: failed to deserialize params: {} from the request {:#?}",
      R::METHOD,
      e,
      self.req
    );
  }

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
        self.log_error::<R>(e);
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
        self.log_error::<R>(e);
        return self;
      }
    };

    let snapshot = self.global.snapshot();

    let responder = self.global.response_sender.clone();
    let id = self.req.id.clone();
    self
      .global
      .pool_sender
      .send(Box::new(move || match f(snapshot, params) {
        Ok(r) => responder
          .send(lsp_server::Message::Response(lsp_server::Response {
            id,
            result: Some(serde_json::to_value(r).unwrap()),
            error: None,
          }))
          .unwrap(),
        Err(_) => responder
          .send(lsp_server::Message::Response(lsp_server::Response {
            id,
            result: None,
            error: Some(lsp_server::ResponseError {
              code:    ErrorCode::RequestCanceled as i32,
              message: "request canceled".to_string(),
              data:    None,
            }),
          }))
          .unwrap(),
      }))
      .unwrap();

    self
  }
}

struct NotificationDispatcher<'a> {
  global: &'a mut GlobalState,
  not:    lsp_server::Notification,
}

impl NotificationDispatcher<'_> {
  fn log_error<N>(&self, e: impl Error)
  where
    N: lsp_types::notification::Notification,
  {
    error!(
      "in notification dispatcher for {}: failed to deserialize params: {} from the notification {:#?}",
      N::METHOD,
      e,
      self.not
    );
  }

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
        self.log_error::<N>(e);
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
  fn log_error<R>(&self, e: impl Error)
  where
    R: scalarc_bsp::types::BspRequest,
  {
    error!(
      "in BSP response dispatcher for {}: failed to deserialize params: {} from the response {:#?}",
      R::METHOD,
      e,
      self.res
    );
  }

  fn on_sync_mut<R>(
    &mut self,
    f: fn(&mut GlobalState, R::Result) -> Result<(), Box<dyn Error>>,
  ) -> &mut Self
  where
    R: scalarc_bsp::types::BspRequest,
  {
    match self.global.bsp_requests.get(&self.res.id) {
      Some(&method) if method == R::METHOD => {
        self.global.bsp_requests.remove(&self.res.id);

        let result = match serde_json::from_value::<R::Result>(self.res.result.clone().unwrap()) {
          Ok(p) => p,
          Err(e) => {
            self.log_error::<R>(e);
            return self;
          }
        };

        f(self.global, result).unwrap();

        self
      }

      _ => self,
    }
  }
}

struct BspNotificationDispatcher<'a> {
  global: &'a mut GlobalState,
  not:    lsp_server::Notification,
}

impl BspNotificationDispatcher<'_> {
  fn log_error<N>(&self, e: impl Error)
  where
    N: scalarc_bsp::types::BspNotification,
  {
    error!(
      "in BSP notification dispatcher for {}: failed to deserialize params: {} from the request {:#?}",
      N::METHOD,
      e,
      self.not
    );
  }

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
        self.log_error::<N>(e);
        return self;
      }
    };
    // TODO: Dispatch this to a thread pool.
    f(self.global, params).unwrap();

    self
  }
}
