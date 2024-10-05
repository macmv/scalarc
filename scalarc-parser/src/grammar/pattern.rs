use crate::CompletedMarker;

use super::*;

pub fn pattern(p: &mut Parser) { pattern_inner(p); }

// Patterns like `1 | 2`
fn pattern_inner(p: &mut Parser) -> Option<CompletedMarker> {
  let mut lhs = atom_pattern(p)?;

  // test ok
  // case 1 | Seq(2, 3) | foo | bar =>
  while p.slice() == "|" {
    let m = lhs.precede(p);
    p.eat(IDENT);
    p.eat_newlines();
    match atom_pattern(p) {
      Some(_) => lhs = m.complete(p, UNION_PATTERN),
      None => {
        m.abandon(p);
        break;
      }
    }
  }
  None
}

// Paterns like `Seq(1)` or `Nil`
fn atom_pattern(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  match p.current() {
    // test ok
    // case 2 | 5 =>
    INT_LIT_KW => {
      p.eat(INT_LIT_KW);
      Some(m.complete(p, LIT_PATTERN))
    }

    // test ok
    // case "hello" | "bye" =>
    DOUBLE_QUOTE => {
      p.eat(DOUBLE_QUOTE);

      expr::double_quote_string(p);

      Some(m.complete(p, DOUBLE_QUOTED_STRING))
    }

    // test ok
    // case (x, y) =>
    T!['('] => {
      arg_pattern(p);
      Some(m.complete(p, TUPLE_PATTERN))
    }

    IDENT => {
      let m2 = p.start();
      p.eat(IDENT);

      if p.at(T![.]) {
        // test ok
        // case foo.bar =>
        p.eat(T![.]);

        loop {
          p.expect(IDENT);

          match p.current() {
            // test ok
            // case foo.bar.baz =>
            T![.] => p.eat(T![.]),

            // test ok
            // case scala.Seq(1, 2) =>
            T!['('] => {
              m2.complete(p, PATH);

              let args = p.start();
              arg_pattern(p);
              args.complete(p, PATTERN_ARGS);
              break Some(m.complete(p, ARG_PATTERN));
            }

            // test ok
            // case foo.bar =>
            _ => {
              m2.complete(p, PATH);

              m.complete(p, PATH_PATTERN);
              break None;
            }
          }
        }
      } else {
        match p.current() {
          // test ok
          // case Seq(1, 2) =>
          T!['('] => {
            m2.complete(p, PATH);

            let args = p.start();
            arg_pattern(p);
            args.complete(p, PATTERN_ARGS);
            Some(m.complete(p, ARG_PATTERN))
          }

          // test ok
          // case foo: Int =>
          T![:] => {
            m2.abandon(p);

            p.eat(T![:]);
            super::type_expr::type_expr_pattern(p);
            Some(m.complete(p, TYPE_PATTERN))
          }

          // test ok
          // case foo @ Int =>
          T![ident] if p.slice() == "@" => {
            m2.abandon(p);

            p.eat(T![ident]);
            pattern(p);
            Some(m.complete(p, AT_PATTERN))
          }

          // test ok
          // case foo =>
          //
          // test ok
          // case foo if bar =>
          //
          // test ok
          // case Seq(foo, bar) =>
          T![=>] | T![if] | T![,] | T![')'] => {
            m2.complete(p, PATH);

            Some(m.complete(p, PATH_PATTERN))
          }

          // `|` is a valid terminator, for union patterns.
          T![ident] if p.slice() == "|" => {
            m2.complete(p, PATH);

            Some(m.complete(p, PATH_PATTERN))
          }

          _ => {
            m2.abandon(p);
            m.abandon(p);

            p.error(format!("expected pattern, got {:?}", p.current()));
            None
          }
        }
      }
    }

    // test err
    // case def =>
    _ => {
      m.abandon(p);
      p.error(format!("expected pattern, got {:?}", p.current()));
      p.recover_until(T![=>]);
      None
    }
  }
}

fn arg_pattern(p: &mut Parser) {
  p.eat(T!['(']);
  // test ok
  // case Seq(
  //   3,
  //   4
  // ) => 1
  p.eat_newlines();

  // test ok
  // case Seq() => 1
  if p.at(T![')']) {
    p.eat(T![')']);
    return;
  }

  loop {
    pattern(p);
    // test ok
    // case Seq(
    //   3
    //   ,
    //   4
    // ) => 1
    p.eat_newlines();
    if p.at(T![,]) {
      p.bump();
      p.eat_newlines();

      // test ok
      // case Seq(3,4,) => 1
      if p.at(T![')']) {
        break;
      }
    } else {
      break;
    }
  }

  p.expect(T![')']);
}
