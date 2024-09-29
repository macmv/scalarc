use crossbeam_channel::Sender;
use lsp_types::{notification::Notification, request::Request};

pub struct ProgressReporter<'a> {
  sender: &'a Sender<lsp_server::Message>,
  token:  u32,
}

impl<'a> ProgressReporter<'a> {
  pub fn new(sender: &'a Sender<lsp_server::Message>, token: u32, title: String) -> Self {
    let reporter = ProgressReporter { sender, token };

    reporter.send_request();
    reporter.report_begin(title);

    reporter
  }

  pub fn update(&self, message: String, percent: f64) {
    self.report_update(message, (percent * 100.0) as u32);
  }

  pub fn finish(self, message: String) { self.report_end(message); }

  fn token(&self) -> lsp_types::ProgressToken {
    lsp_types::ProgressToken::Number(self.token as i32)
  }

  fn send_request(&self) {
    self
      .sender
      .send(lsp_server::Message::Request(lsp_server::Request {
        // Honestly we don't really care about the response, so just pick a big ID.
        id:     547825783.into(),
        method: lsp_types::request::WorkDoneProgressCreate::METHOD.into(),
        params: serde_json::to_value(lsp_types::WorkDoneProgressCreateParams {
          token: self.token(),
        })
        .unwrap(),
      }))
      .unwrap();
  }

  fn report_begin(&self, title: String) {
    self
      .sender
      .send(lsp_server::Message::Notification(lsp_server::Notification {
        method: lsp_types::notification::Progress::METHOD.into(),
        params: serde_json::to_value(lsp_types::ProgressParams {
          token: self.token(),
          value: lsp_types::ProgressParamsValue::WorkDone(lsp_types::WorkDoneProgress::Begin(
            lsp_types::WorkDoneProgressBegin {
              title,
              percentage: None,
              cancellable: Some(false),
              message: None,
            },
          )),
        })
        .unwrap(),
      }))
      .unwrap();
  }

  fn report_update(&self, message: String, percent: u32) {
    self
      .sender
      .send(lsp_server::Message::Notification(lsp_server::Notification {
        method: lsp_types::notification::Progress::METHOD.into(),
        params: serde_json::to_value(lsp_types::ProgressParams {
          token: self.token(),
          value: lsp_types::ProgressParamsValue::WorkDone(lsp_types::WorkDoneProgress::Report(
            lsp_types::WorkDoneProgressReport {
              cancellable: Some(false),
              message:     Some(message),
              percentage:  Some(percent),
            },
          )),
        })
        .unwrap(),
      }))
      .unwrap();
  }

  fn report_end(&self, message: String) {
    self
      .sender
      .send(lsp_server::Message::Notification(lsp_server::Notification {
        method: lsp_types::notification::Progress::METHOD.into(),
        params: serde_json::to_value(lsp_types::ProgressParams {
          token: self.token(),
          value: lsp_types::ProgressParamsValue::WorkDone(lsp_types::WorkDoneProgress::End(
            lsp_types::WorkDoneProgressEnd { message: Some(message) },
          )),
        })
        .unwrap(),
      }))
      .unwrap();
  }
}
