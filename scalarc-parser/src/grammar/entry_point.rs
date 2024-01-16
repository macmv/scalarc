use super::*;

pub fn source_file(p: &mut Parser) {
  let m = p.start();
  items::mod_items(p);
  m.complete(p, SOURCE_FILE);
}
