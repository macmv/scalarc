use super::*;

pub fn mod_items(p: &mut Parser) {
  while !(p.at(EOF)) {
    item(p);
  }
}

fn item(p: &mut Parser) {
  let m = p.start();

  match p.current() {
    T![import] => import_item(p, m),

    /*
    T![type] => type_alias(p, m),
    T![struct] => adt::strukt(p, m),
    T![enum] => adt::enum_(p, m),
    IDENT if p.at_contextual_kw(T![union]) && p.nth(1) == IDENT => adt::union(p, m),

    T![macro] => macro_def(p, m),
    IDENT if p.at_contextual_kw(T![macro_rules]) && p.nth(1) == BANG => macro_rules(p, m),

    T![const] if (la == IDENT || la == T![_] || la == T![mut]) => consts::konst(p, m),
    T![static] if (la == IDENT || la == T![_] || la == T![mut]) => consts::static_(p, m),
    */
    _ => {
      m.abandon(p);
    }
  };
}

fn import_item(p: &mut Parser, m: Marker) {
  p.eat(T![import]);
  // TODO: Parse the tree
  p.expect(T![nl]);
  m.complete(p, IMPORT);
}
