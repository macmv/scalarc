use item::lambda_items;

use crate::{CompletedMarker, Marker};

use super::*;

fn op_bp(ident: &str) -> (u8, u8) {
  // See https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html
  let precedence = match ident {
    s if s.chars().all(|c| c.is_ascii_alphabetic() || c == '_') => 10,
    "|" => 9,
    "^" => 8,
    "&" => 7,
    "=" | "!" => 6,
    "<" | ">" => 5,
    ":" => 4,
    "+" | "-" => 3,
    "*" | "/" | "%" => 2,
    _ => 1,
  };

  if ident.ends_with(':') {
    // Right-associative
    (precedence, precedence)
  } else {
    // Left-associative
    (precedence, precedence + 1)
  }
}

pub fn expr(p: &mut Parser) { expr_bp(p, 0, true); }

// This is primarily used for parsing `if` predicates in `case` expressions,
// where a `=>` should be interpretted as a terminator, and not a lambda.
pub fn expr_no_fat_arrow(p: &mut Parser) { expr_bp(p, 0, false); }

fn expr_bp(p: &mut Parser, min_bp: u8, fat_arrow: bool) {
  let Some(mut lhs) = simple_expr(p) else {
    return;
  };

  loop {
    // test ok
    // foo = 3
    if p.current() == T![ident]
      || p.current() == T![=]
      || p.current() == T![:]
      || (p.current() == T![=>] && fat_arrow)
    {
      let op_tok = p.current();
      let kind = if op_tok == T![=] { ASSIGN_EXPR } else { INFIX_EXPR };

      let op = p.slice();
      let (l_bp, r_bp) = op_bp(op);
      if l_bp < min_bp {
        return;
      }

      let m = lhs.precede(p);
      p.bump(); // eat T![ident] or T![=>]

      let is_at_terminator = match p.current() {
        T![,] | T![')'] | T!['}'] | T![else] | EOF => true,
        _ => false,
      };

      // this is one expression
      // 2 +
      //   3
      //
      // these are two expressions
      // 2 +
      //
      //   3
      if (p.eat_newlines_max(1) >= 1 && p.at(T![nl])) || is_at_terminator {
        if kind == ASSIGN_EXPR {
          m.abandon(p);
          p.error(format!("expected expression, got expression separator {:?}", p.current()));
          p.recover_until_any(&[T![nl], T![,], T![')'], T!['}']]);
          return;
        } else {
          m.complete(p, POSTFIX_EXPR);
          return;
        }
      } else {
        if op_tok == T![=>] {
          // test ok
          // { foo =>
          //   val x = foo
          //   x
          // }
          //
          // test ok
          // println(foo => 2 + 3)
          {
            let m = p.start();
            lambda_items(p);
            m.complete(p, BLOCK);
          }

          m.complete(p, LAMBDA_EXPR);
          return;
        } else if op_tok == T![:] {
          ascription(p);
          m.complete(p, ASCRIPT_EXPR);
          return;
        } else {
          expr_bp(p, r_bp, fat_arrow);
          lhs = m.complete(p, kind);
        }
      }
    } else {
      match p.current() {
        T![nl] | T![=>] | T![,] | T![')'] | T!['}'] | T![else] | EOF => {
          return;
        }

        _ => {
          p.error(format!("expected operator, got {:?}", p.current()));
          p.recover_until_any(&[T![nl], T![,], T![')'], T!['}'], T![else]]);
          return;
        }
      }
    }
  }
}

// test ok
// foo: Int
// foo: _*
fn ascription(p: &mut Parser) {
  let m = p.start();

  if p.at(T![ident]) && p.slice() == "_" {
    p.eat(T![ident]);
    if p.at(T![ident]) && p.slice() == "*" {
      p.eat(T![ident]);
      m.complete(p, SPREAD_ASCRIPTION);
    } else {
      // FIXME: This isn't handled correctly.
      p.error("expected `*`");
      m.abandon(p);
    }
  } else {
    type_expr::type_expr(p);
    m.complete(p, TYPE_ASCRIPTION);
  }
}

// Expressions like `hello(3, 4)`
fn simple_expr(p: &mut Parser) -> Option<CompletedMarker> {
  let prefix = prefix_expr(p);
  let m = p.start();
  let lhs = match atom_expr(p, m) {
    Some(m) => m,
    None => {
      if let Some(prefix) = prefix {
        prefix.abandon(p);
      }
      return None;
    }
  };
  let m = postfix_expr(p, lhs);
  match prefix {
    Some(prefix) => Some(prefix.complete(p, PREFIX_EXPR)),
    None => Some(m),
  }
}

// test ok
// !true
// ~3
fn prefix_expr(p: &mut Parser) -> Option<Marker> {
  if p.current() != T![ident] {
    return None;
  }

  match p.slice() {
    "+" | "-" | "~" | "!" => {
      let m = p.start();
      p.eat(T![ident]);
      Some(m)
    }

    _ => None,
  }
}

fn postfix_expr(p: &mut Parser, mut lhs: CompletedMarker) -> CompletedMarker {
  loop {
    lhs = match p.current() {
      // test ok
      // hi(3)
      T!['('] => {
        let call = lhs.precede(p);

        let m = p.start();
        call_paren_expr(p);
        m.complete(p, PAREN_ARGUMENTS);

        call.complete(p, CALL_EXPR)
      }
      // test ok
      // hi[Int]
      T!['['] => {
        let te = lhs.precede(p);

        {
          let m = p.start();
          type_expr::type_args(p, T!['['], T![']']);
          m.complete(p, TYPE_ARGS);
        }

        te.complete(p, TYPED_EXPR)
      }
      // test ok
      // hi { 3 }
      T!['{'] => {
        let m = lhs.precede(p);
        call_block_expr(p);
        m.complete(p, CALL_EXPR)
      }
      // test ok
      // foo.bar
      T![.] => match postfix_dot_expr(p, lhs) {
        Ok(it) => it,
        Err(it) => {
          lhs = it;
          break;
        }
      },

      // test ok
      // foo
      //   .bar
      //   .baz
      T![nl] => match p.peek() {
        T![.] => {
          p.eat(T![nl]);
          match postfix_dot_expr(p, lhs) {
            Ok(it) => it,
            Err(it) => {
              lhs = it;
              break;
            }
          }
        }
        _ => break,
      },

      T![match] => match_expr(p, lhs),

      _ => break,
    };
  }
  lhs
}

// test ok
// 2 match {
//   case 3 => 4
// }
fn match_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
  let m = lhs.precede(p);

  p.eat(T![match]);

  p.expect(T!['{']);
  p.eat_newlines();

  if p.at(T!['}']) {
    p.error_bump("unexpected end of match block");
    return m.complete(p, MATCH_EXPR);
  }

  loop {
    if p.at(EOF) {
      p.error("unexpected end of file");
      break;
    }

    let c = p.start();

    // case_item eats a newline and handles errors.
    super::item::case_item(p, c);

    p.eat_newlines();

    if p.at(T!['}']) {
      p.eat(T!['}']);
      break;
    }
  }

  m.complete(p, MATCH_EXPR)
}

pub fn call_paren_expr(p: &mut Parser) {
  p.eat(T!['(']);
  // test ok
  // hi(
  //   3,
  //   4
  // )
  p.eat_newlines();

  // test ok
  // hi()
  if p.at(T![')']) {
    p.eat(T![')']);
    return;
  }

  loop {
    // FIXME: I think this might run into ambiguity with assignments?.
    if p.current() == T![ident] && p.peek() == T![=] {
      // test ok
      // hi(
      //   foo = 3,
      //   bar = 4
      // )
      p.expect(T![ident]);
      p.expect(T![=]);
      expr(p);
    } else {
      expr(p);
    }

    // test ok
    // hi(
    //   3
    //   ,
    //   4
    // )
    p.eat_newlines();
    if p.at(T![,]) {
      p.bump();
      p.eat_newlines();

      // test ok
      // hi(3,4,)
      if p.at(T![')']) {
        break;
      }
    } else {
      break;
    }
  }

  p.expect(T![')']);
}

fn tuple_expr(p: &mut Parser) {
  p.eat(T!['(']);
  // test ok
  // (
  //   3,
  //   4
  // )
  p.eat_newlines();

  // test ok
  // ()
  if p.at(T![')']) {
    p.eat(T![')']);
    return;
  }

  loop {
    expr(p);
    // test ok
    // (
    //   3
    //   ,
    //   4
    // )
    p.eat_newlines();
    if p.at(T![,]) {
      p.bump();
      p.eat_newlines();

      // test ok
      // (3,4,)
      if p.at(T![')']) {
        break;
      }
    } else {
      break;
    }
  }

  p.expect(T![')']);
}

fn call_block_expr(p: &mut Parser) {
  let m = p.start();
  // test ok
  // hi {
  //   3
  //   4
  // }
  super::item::block_items(p);
  m.complete(p, BLOCK_EXPR);
}

fn postfix_dot_expr(
  p: &mut Parser,
  lhs: CompletedMarker,
) -> Result<CompletedMarker, CompletedMarker> {
  match p.peek() {
    T![ident] => {
      let m = lhs.precede(p);
      p.eat(T![.]);
      p.eat(T![ident]);
      Ok(m.complete(p, FIELD_EXPR))
    }
    _ => {
      let m = lhs.precede(p);
      p.eat(T![.]);
      p.error(format!("expected identifier, got {:?}", p.current()));
      Err(m.complete(p, FIELD_EXPR))
    }
  }
}

// Expressions like `1` or "hi"
fn atom_expr(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
  match p.current() {
    INT_LIT_KW => {
      p.eat(INT_LIT_KW);
      Some(m.complete(p, LIT_EXPR))
    }

    T![true] => {
      p.eat(T![true]);
      Some(m.complete(p, LIT_EXPR))
    }
    T![false] => {
      p.eat(T![false]);
      Some(m.complete(p, LIT_EXPR))
    }

    SINGLE_QUOTE => {
      p.eat(SINGLE_QUOTE);

      character_lit(p);

      Some(m.complete(p, CHARACTER_LIT))
    }

    DOUBLE_QUOTE => {
      p.eat(DOUBLE_QUOTE);

      double_quote_string(p);

      Some(m.complete(p, DOUBLE_QUOTED_STRING))
    }

    TRIPPLE_QUOTE => {
      p.eat(TRIPPLE_QUOTE);

      tripple_quote_string(p);

      Some(m.complete(p, TRIPPLE_QUOTED_STRING))
    }

    IDENT => {
      p.eat(IDENT);

      match p.current() {
        // test ok
        // s"hello $world"
        DOUBLE_QUOTE => {
          p.eat(DOUBLE_QUOTE);

          // FIXME: Need to parse interpolated strings.
          double_quote_string(p);

          Some(m.complete(p, DOUBLE_QUOTED_STRING))
        }

        // test ok
        // s"""hello $world"""
        TRIPPLE_QUOTE => {
          p.eat(TRIPPLE_QUOTE);

          // FIXME: Need to parse interpolated strings.
          tripple_quote_string(p);

          Some(m.complete(p, TRIPPLE_QUOTED_STRING))
        }
        _ => Some(m.complete(p, IDENT_EXPR)),
      }
    }

    T![return] => {
      // test ok
      // return 3
      p.eat(T![return]);
      // test ok
      // return
      if !p.at(T![nl]) {
        expr(p);
      }
      Some(m.complete(p, RETURN_EXPR))
    }

    T![throw] => {
      // test ok
      // throw new IllegalStateException(3, 4)
      p.eat(T![throw]);
      expr(p);
      Some(m.complete(p, THROW_EXPR))
    }

    T![new] => {
      p.eat(T![new]);

      // test ok
      // new Iterator
      if p.at(T![ident]) {
        p.eat(T![ident]);
      }

      // test ok
      // new Iterator[Int]
      // new Iterator[String]("hello")
      if p.at(T!['[']) {
        let m = p.start();
        super::type_expr::type_args(p, T!['['], T![']']);
        m.complete(p, TYPE_ARGS);
      }

      // test ok
      // new Iterator()
      // new Iterator("hello")
      if p.at(T!['(']) {
        let m = p.start();
        call_paren_expr(p);
        m.complete(p, PAREN_ARGUMENTS);
      }

      // test ok
      // new { def next = None }
      // new Iterator { def next = None }
      if p.at(T!['{']) {
        call_block_expr(p);
      }

      Some(m.complete(p, NEW_EXPR))
    }

    T!['{'] => {
      // test ok
      // {
      //   2 + 3
      //   5 + 6
      // }
      super::item::block_items(p);
      Some(m.complete(p, BLOCK_EXPR))
    }

    T!['('] => {
      // test ok
      // ()
      // (1 to 100)
      // (1, 2)
      tuple_expr(p);
      Some(m.complete(p, TUPLE_EXPR))
    }

    T![if] => {
      if_expr(p);
      Some(m.complete(p, IF_EXPR))
    }

    _ => {
      m.abandon(p);
      p.error(format!("expected expression, got {:?}", p.current()));
      p.recover_until(T![nl]);
      None
    }
  }
}

pub fn character_lit(p: &mut Parser) {
  // test ok
  // 'a'
  // '*'
  match p.current() {
    // test err
    // ''
    SINGLE_QUOTE => {
      p.error("unexpected empty character literal");
      p.eat(SINGLE_QUOTE);
      return;
    }
    // test err
    // 'a
    EOF => {
      p.error("unexpected end of file");
      return;
    }

    // test ok
    // '3'
    _ => {
      // test err
      // 'ab'
      if p.slice().chars().count() > 1 {
        p.error("expected a single character");
      }
      p.bump();
    }
  }

  match p.current() {
    SINGLE_QUOTE => {
      p.eat(SINGLE_QUOTE);
    }

    _ => {
      p.error("expected a single character");
      p.bump();
    }
  }
}

pub fn double_quote_string(p: &mut Parser) {
  // test ok
  // "hello"
  loop {
    match p.current() {
      DOUBLE_QUOTE => {
        p.eat(DOUBLE_QUOTE);
        break;
      }
      // test err
      // "hello
      EOF => {
        p.error("unexpected end of file");
        break;
      }
      // test ok
      // "hello\"world"
      IDENT if p.slice() == "\\" => {
        p.bump();

        // TODO: Parse unicode escapes and such.
        p.bump();
      }
      _ => {
        p.bump();
      }
    }
  }
}

pub fn tripple_quote_string(p: &mut Parser) {
  // test ok
  // """hello"""
  loop {
    match p.current() {
      TRIPPLE_QUOTE => {
        p.eat(TRIPPLE_QUOTE);
        break;
      }
      // test err
      // """hello
      EOF => {
        p.error("unexpected end of file");
        break;
      }
      // test ok
      // """hello
      // world"""
      _ => {
        p.bump();
      }
    }
  }
}

// test ok
// if (true) 3
fn if_expr(p: &mut Parser) {
  p.eat(T![if]);

  p.expect(T!['(']);
  expr(p);
  p.expect(T![')']);

  expr(p);

  // if (true) 3 else 4
  if p.at(T![else]) {
    p.eat(T![else]);
    expr(p);
  }
}
