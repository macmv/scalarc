use crate::CompletedMarker;

use super::*;

/// A pattern terminated by `=` or `)`.
pub fn pattern_val(p: &mut Parser) { pattern_inner(p, false); }
/// A pattern terminated by `=>`.
pub fn pattern_case(p: &mut Parser) { pattern_inner(p, true); }

// Patterns like `1 | 2`
fn pattern_inner(p: &mut Parser, is_case: bool) -> Option<CompletedMarker> {
  let mut lhs = atom_pattern(p, is_case)?;

  while p.at(T![ident]) {
    if p.slice() == "|" {
      // test ok
      // case 1 | Seq(2, 3) | foo | bar =>
      let m = lhs.precede(p);
      p.eat(T![ident]);
      p.eat_newlines();
      match atom_pattern(p, is_case) {
        Some(_) => lhs = m.complete(p, UNION_PATTERN),
        None => {
          m.abandon(p);
          break;
        }
      }
    } else {
      // test ok
      // case 1 :: 2 :: Nil =>
      let m = lhs.precede(p);
      p.eat(T![ident]);
      p.eat_newlines();
      match atom_pattern(p, is_case) {
        Some(_) => lhs = m.complete(p, INFIX_PATTERN),
        None => {
          m.abandon(p);
          break;
        }
      }
    }
  }

  None
}

// Paterns like `Seq(1)` or `Nil`
fn atom_pattern(p: &mut Parser, is_case: bool) -> Option<CompletedMarker> {
  let m = p.start();

  match p.current() {
    // test ok
    // case 2 | 5 | 3.0 | true | false =>
    INT_LIT_KW | FLOAT_LIT_KW | T![true] | T![false] => {
      p.bump();
      Some(m.complete(p, LIT_PATTERN))
    }

    // test ok
    // case -5 =>
    T![ident] if p.slice() == "-" && p.peek() == INT_LIT_KW => {
      p.eat(T![ident]); // The `-`
      p.eat(INT_LIT_KW);
      Some(m.complete(p, LIT_PATTERN))
    }

    // test ok
    // case +5 =>
    T![ident] if p.slice() == "+" && p.peek() == INT_LIT_KW => {
      p.eat(T![ident]); // The `+`
      p.eat(INT_LIT_KW);
      Some(m.complete(p, LIT_PATTERN))
    }

    // test ok
    // case '+' | '-' =>
    SINGLE_QUOTE => {
      expr::character_lit(p);

      Some(m.complete(p, CHARACTER_LIT))
    }

    // test ok
    // case "hello" | "bye" =>
    DOUBLE_QUOTE => {
      expr::parse_string(p, false);

      Some(m.complete(p, DOUBLE_QUOTED_STRING))
    }

    // test ok
    // case s"hello" =>
    T![ident] if p.peek() == DOUBLE_QUOTE => {
      p.eat(T![ident]);

      expr::parse_string(p, false);

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

      let mut is_path = false;
      while p.at(T![.]) {
        // test ok
        // case foo.bar =>
        p.eat(T![.]);
        p.expect(T![ident]);
        is_path = true;
      }

      // test ok
      // case _
      //   =>
      //
      // test ok
      // case _
      //   if true =>
      p.eat_newlines();

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
        T![:] if !is_path => {
          m2.abandon(p);

          p.eat(T![:]);
          super::type_expr::simple_type_expr_is_case(p, is_case);
          Some(m.complete(p, TYPE_PATTERN))
        }

        // test ok
        // case foo @ Int =>
        T![@] if !is_path => {
          m2.abandon(p);

          p.eat(T![@]);

          // test ok
          // case foo @ _* =>
          if p.at(T![ident]) && p.slice() == "_" {
            let m2 = p.start();
            p.eat(T![ident]);
            if p.at(T![ident]) && p.slice() == "*" {
              m2.abandon(p);
              p.eat(T![ident]);
              Some(m.complete(p, SPREAD_PATTERN))
            } else {
              // test ok
              // case foo @ _ =>

              m2.complete(p, PATH_PATTERN);
              Some(m.complete(p, AT_PATTERN))
            }
          } else {
            pattern_inner(p, is_case);
            Some(m.complete(p, AT_PATTERN))
          }
        }

        // test ok
        // case foo =>
        //
        // test ok
        // case foo if bar =>
        //
        // test ok
        // case Seq(foo, bar) =>
        _ => {
          m2.complete(p, PATH);

          Some(m.complete(p, PATH_PATTERN))
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
    pattern_val(p);
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
