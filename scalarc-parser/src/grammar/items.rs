use super::*;

pub fn mod_items(p: &mut Parser) {
  while !p.at(EOF) {
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
      p.error_bump(format!("expected item, got {:?}", p.current()));
      m.abandon(p);
    }
  };
}

// test
// ---
// import foo.bar.baz
// ---
// SOURCE_FILE
//   IMPORT
//     IMPORT_KW
//     IDENT
//     DOT
//     IDENT
//     DOT
//     IDENT
//     NL_KW
fn import_item(p: &mut Parser, m: Marker) {
  p.eat(T![import]);
  loop {
    match p.current() {
      T![ident] => {
        p.bump();
      }

      T![.] => {
        p.bump();
        continue;
      }

      T!['{'] => import_list(p),

      T![nl] => {
        p.eat(T![nl]);
        m.complete(p, IMPORT);
        return;
      }

      // test
      // ---
      // import 3
      // ---
      // SOURCE_FILE
      //   IMPORT_KW
      //   error: expected ident, got LITERAL
      //   LITERAL
      //   NL_KW
      _ => {
        p.error(format!("expected ident, got {:?}", p.current()));
        p.recover_until(T![nl]);
        m.abandon(p);
        return;
      }
    }
  }
}

// test
// ---
// import foo.{ bar, baz }
// ---
// SOURCE_FILE
//   IMPORT
//     IMPORT_KW
//     IDENT
//     DOT
//     IMPORT_SELECTORS
//       OPEN_CURLY
//       IDENT
//       COMMA
//       IDENT
//       CLOSE_CURLY
//     NL_KW
fn import_list(p: &mut Parser) {
  let m = p.start();
  p.eat(T!['{']);
  loop {
    match p.current() {
      T![ident] => p.eat(T![ident]),
      T![,] => p.eat(T![,]),

      T!['}'] => {
        p.eat(T!['}']);
        break;
      }

      _ => break,
    }
  }

  m.complete(p, IMPORT_SELECTORS);
}
