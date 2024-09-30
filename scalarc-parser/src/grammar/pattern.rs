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
    atom_pattern(p)?;
    lhs = m.complete(p, UNION_PATTERN);
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

    IDENT => {
      p.eat(IDENT);

      match p.current() {
        // test ok
        // case Seq(1, 2) =>
        T!['('] => {
          arg_pattern(p);
          Some(m.complete(p, ARG_PATTERN))
        }

        // test ok
        // case foo: Int =>
        T![:] => {
          p.eat(T![:]);
          super::type_expr::type_expr(p);
          Some(m.complete(p, TYPE_PATTERN))
        }

        // test ok
        // case foo @ Int =>
        T![ident] if p.slice() == "@" => {
          p.eat(T![ident]);
          pattern(p);
          Some(m.complete(p, AT_PATTERN))
        }

        // test ok
        // case foo =>
        _ => Some(m.complete(p, IDENT_PATTERN)),
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
  let args = p.start();
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
    args.complete(p, PATTERN_ARGS);
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
  args.complete(p, PATTERN_ARGS);
}
