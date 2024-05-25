#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(pub(crate) String);

impl Name {
  pub fn new(s: String) -> Self { Name(s) }
  pub fn as_str(&self) -> &str { &self.0 }
  pub fn into_string(self) -> String { self.0 }
}

impl From<String> for Name {
  fn from(s: String) -> Self { Name(s.to_owned()) }
}
impl From<&str> for Name {
  fn from(s: &str) -> Self { Name(s.to_owned()) }
}
