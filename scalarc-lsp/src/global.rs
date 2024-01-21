//! Handles global state and the main loop of the server.

use crossbeam_channel::{Receiver, Sender};
use std::error::Error;

use lsp_types::Url;

pub struct GlobalState {
  pub sender:    Sender<lsp_server::Message>,
  pub workspace: Url,
}

impl GlobalState {
  pub fn run(self, receiver: Receiver<lsp_server::Message>) -> Result<(), Box<dyn Error>> {
    // TODO: Spawn a task to fetch bsp status here.

    while let Ok(ev) = receiver.recv() {
      self.handle_event(ev);
    }

    Ok(())
  }

  fn handle_event(&self, ev: lsp_server::Message) {
    match ev {
      lsp_server::Message::Request(req) => {
        let _ = self.handle_request(req);
      }
      lsp_server::Message::Notification(not) => {
        let _ = self.handle_notification(not);
      }
      lsp_server::Message::Response(_) => (),
    }
  }

  fn handle_request(&self, req: lsp_server::Request) -> Result<(), Box<dyn Error>> {
    let mut dispatcher = RequestDispatcher { global: &self, req };

    use crate::handler::request;
    use lsp_types::request as lsp_request;

    dispatcher.on::<lsp_request::Completion>(request::handle_completion);

    Ok(())
  }

  fn handle_notification(&self, not: lsp_server::Notification) -> Result<(), Box<dyn Error>> {
    let mut dispatcher = NotificationDispatcher { global: &self, not };

    use crate::handler::notification;
    use lsp_types::notification as lsp_notification;

    dispatcher
      .on::<lsp_notification::DidOpenTextDocument>(notification::handle_open_text_document)
      .on::<lsp_notification::DidChangeTextDocument>(notification::handle_change_text_document);

    Ok(())
  }
}

struct RequestDispatcher<'a> {
  global: &'a GlobalState,
  req:    lsp_server::Request,
}

impl RequestDispatcher<'_> {
  fn on<R>(&mut self, f: fn(R::Params) -> Result<R::Result, Box<dyn Error>>) -> &mut Self
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
    let response = f(params).unwrap();
    self
      .global
      .sender
      .send(lsp_server::Message::Response(lsp_server::Response {
        id:     self.req.id.clone(),
        result: Some(serde_json::to_value(response).unwrap()),
        error:  None,
      }))
      .unwrap();

    self
  }
}

struct NotificationDispatcher<'a> {
  global: &'a GlobalState,
  not:    lsp_server::Notification,
}

impl NotificationDispatcher<'_> {
  fn on<N>(&mut self, f: fn(N::Params) -> Result<(), Box<dyn Error>>) -> &mut Self
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
    f(params).unwrap();

    self
  }
}
