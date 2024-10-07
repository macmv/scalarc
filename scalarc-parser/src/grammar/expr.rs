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

      // Eat at most 1 newline.
      // This is one expression:
      // 2 +
      //   3
      //
      // These are two expressions:
      // 2 +
      //
      //   3
      let found_newline = p.at(T![nl]);
      let terminator_token = if found_newline { p.peek() } else { p.current() };

      // Any of these tokens are a terminator, or a double newline is a terminator.
      let is_at_terminator = match terminator_token {
        // test ok
        // foo match {
        //   case x => x flatten
        //   case y => y flatten
        // }
        T![,]
        | T![')']
        | T!['}']
        | T![case]
        | T![else]
        | T![catch]
        | T![finally]
        | T![while]
        | EOF => true,
        T![nl] if found_newline => true,

        // `val` and `var` declare the next statement, but in the case of a lambda, `val` and `var`
        // are just part of the block.
        T![val] | T![var] | T![def] | T![override] if op_tok != T![=>] => true,

        _ => false,
      };

      if is_at_terminator {
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
        if found_newline {
          p.eat(T![nl]);
        }

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
          // test ok
          // { foo: Int => foo }
          lhs = m.complete(p, ASCRIPT_EXPR);
        } else {
          expr_bp(p, r_bp, fat_arrow);
          lhs = m.complete(p, kind);
        }
      }
    } else {
      match p.current() {
        T![nl]
        | T![=>]
        | T![,]
        | T![')']
        | T!['}']
        | T![case]
        | T![if]
        | T![else]
        | T![catch]
        | T![finally]
        | T![while]
        | EOF => {
          return;
        }

        _ => {
          p.error(format!("expected operator, got {:?}", p.current()));
          p.recover_until_any(&[
            T![nl],
            T![,],
            T![')'],
            T!['}'],
            T![case],
            T![else],
            T![catch],
            T![finally],
            T![while],
          ]);
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
  } else if p.at(T![@]) {
    // test ok
    // (foo: @unchecked) match {
    //   case _ => ()
    // }
    super::item::annotation(p);
    m.complete(p, ANNOTATION_ASCRIPTION);
  } else {
    type_expr::simple_type_expr_is_case(p, true);
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
    // Eat enough newlines to see the next real token.
    while p.at(T![nl]) && p.peek() == T![nl] {
      p.eat(T![nl]);
    }

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
      T![.] => {
        p.eat(T![.]);

        // test ok
        // foo.
        //   bar
        if p.at(T![nl]) && p.peek() == T![ident] {
          p.eat(T![nl]);
        }

        match postfix_dot_expr(p, lhs) {
          Ok(it) => it,
          Err(it) => {
            lhs = it;
            break;
          }
        }
      }

      // test ok
      // foo
      //   .bar
      //   .baz
      T![nl] if p.peek() == T![.] => {
        p.eat(T![nl]);

        match p.current() {
          T![.] => {
            p.eat(T![.]);
            match postfix_dot_expr(p, lhs) {
              Ok(it) => it,
              Err(it) => {
                lhs = it;
                break;
              }
            }
          }
          _ => break,
        }
      }

      T![match] => match_expr(p, lhs),

      // test ok
      // val foo = myLambda _
      T![ident] if p.slice() == "_" => {
        let m = lhs.precede(p);
        p.eat(T![ident]);
        // TODO: This name is too long, it needs a better name.
        m.complete(p, POSTFIX_UNDERSCORE_EXPR)
      }

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
      // test ok
      // hi(
      //   foo =
      //     3,
      //   bar = 4
      // )
      p.eat_newlines();
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
  match p.current() {
    T![ident] => {
      let m = lhs.precede(p);
      p.eat(T![ident]);
      Ok(m.complete(p, FIELD_EXPR))
    }
    _ => {
      let m = lhs.precede(p);
      p.error(format!("expected identifier, got {:?}", p.current()));
      Err(m.complete(p, FIELD_EXPR))
    }
  }
}

// Expressions like `1` or "hi"
fn atom_expr(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
  match p.current() {
    // test ok
    // 2 == 2L == 3l
    INT_LIT_KW => {
      p.eat(INT_LIT_KW);

      Some(m.complete(p, LIT_EXPR))
    }

    // test ok
    // 2 == 2.0D == 3d
    FLOAT_LIT_KW => {
      p.bump();

      Some(m.complete(p, LIT_EXPR))
    }

    // test ok
    // true != false
    T![true] | T![false] => {
      p.bump();
      Some(m.complete(p, LIT_EXPR))
    }

    SINGLE_QUOTE => {
      character_lit(p);

      Some(m.complete(p, CHARACTER_LIT))
    }

    DOUBLE_QUOTE => {
      parse_string(p, false);

      Some(m.complete(p, DOUBLE_QUOTED_STRING))
    }

    IDENT => {
      p.eat(IDENT);

      match p.current() {
        // test ok
        // s"hello $world"
        // s"""hello $world"""
        DOUBLE_QUOTE => {
          parse_string(p, true);

          Some(m.complete(p, INTERPOLATED_STRING))
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

        // test ok
        // new foo.bar.Iterator()
        while p.at(T![.]) {
          p.eat(T![.]);
          p.expect(T![ident]);
        }
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
      // new Iterator() with Seq[Int]
      if p.at(T![with]) {
        p.eat(T![with]);
        super::type_expr::simple_type_expr(p);
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

    T![while] => {
      while_expr(p);
      Some(m.complete(p, WHILE_EXPR))
    }

    T![do] => {
      do_while_expr(p);
      Some(m.complete(p, DO_WHILE_EXPR))
    }

    T![for] => {
      for_expr(p);
      Some(m.complete(p, FOR_EXPR))
    }

    T![try] => {
      try_expr(p);
      Some(m.complete(p, TRY_EXPR))
    }

    // NB: This should only really be allowed as the item of a `def`, but this works well enough.
    T![macro] => {
      macro_expr(p);
      Some(m.complete(p, MACRO_EXPR))
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
  // val x = println('[')
  // val y = 3
  p.set_in_string(true);

  p.eat(SINGLE_QUOTE);

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

    // test ok
    // ' '

    // test err
    // 'a
    EOF => {
      p.error("unexpected end of file");
      return;
    }

    // test ok
    // '\\'
    // '\u0000'
    T![ident] if p.slice() == "\\" => {
      p.eat(T![ident]);
      match p.slice() {
        "u" => {
          p.bump(); // eat `u`

          // Eat the number.
          //
          // FIXME: This is wrong if the number literal is too long. Need to change the
          // tokenizer to not parse numbers.
          let mut chars = 0;
          while chars < 4 {
            chars += p.slice().len();
            p.bump();
          }
        }
        "\"" | "'" | "\\" | "n" | "r" | "t" | "b" | "f" => {
          p.bump(); // eat the escape
        }
        _ => {
          p.error("unknown escape code");
          // Assume a single letter.
          p.bump();
        }
      }
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
      // test ok
      // val x = println('h')
      // val y = 3
      p.set_in_string(false);
      p.eat(SINGLE_QUOTE);
    }

    _ => {
      p.error("expected a single character");
      p.bump();
    }
  }
}

pub fn parse_string(p: &mut Parser, interpolations: bool) {
  let mut is_start = true;
  let mut is_tripple_quote = false;
  let mut quote_count = 0;

  // test ok
  // val x = println(s"[im in brackets]")
  // val y = 3
  p.set_in_string(true);

  p.eat(DOUBLE_QUOTE);

  loop {
    match p.current() {
      DOUBLE_QUOTE if is_tripple_quote => {
        p.eat(DOUBLE_QUOTE);
        quote_count += 1;

        // This makes a string with the text `"hello"`.
        //
        // test ok
        // """"hello""""
        dbg!(quote_count);
        if quote_count >= 3 && !p.at(DOUBLE_QUOTE) {
          p.set_in_string(false);
          break;
        }

        continue;
      }
      _ => quote_count = 0,
    }

    match p.current() {
      DOUBLE_QUOTE if !is_tripple_quote => {
        // test ok
        // val x = println("hello")
        // val y = 3
        p.set_in_string(false);
        p.eat(DOUBLE_QUOTE);

        if is_start && p.at(DOUBLE_QUOTE) {
          p.eat(DOUBLE_QUOTE);
          p.set_in_string(true);
          is_tripple_quote = true;
        } else {
          break;
        }
      }

      T![ident] if interpolations && p.slice() == "$" => {
        let m = p.start();
        p.eat(T![ident]); // The `$`

        if p.at(T![ident]) && p.slice() == "$" {
          // test ok
          // val x = println(s"$${im still a string}")
          m.abandon(p);
          p.eat(T![ident]);
        } else if p.at(T!['{']) {
          p.set_in_string(false);
          p.eat(T!['{']);

          // test ok
          // val x = s"${
          //   val foo = 3
          //   foo + 4
          // }"
          super::item::lambda_items(p);

          // HACK: We set this back to `in_string`, so that `p.expect()` can parse the
          // next token as being inside a string. Then, we also pop the `}`
          // manually.
          p.set_in_string(true);
          if p.current() == T!['}'] {
            p.brace_stack.pop();
            p.bump();
          } else {
            p.expect(T!['}']);
          }

          m.complete(p, INTERPOLATION);
        } else {
          // TODO: Ident interpolations.
          m.abandon(p);
        }
      }

      // test err
      // "hello
      EOF => {
        p.error("unexpected end of file");
        break;
      }

      // test ok
      // "hello\"world"
      // "\\\""
      T![ident] if p.slice() == "\\" && !is_tripple_quote => {
        p.eat(T![ident]);
        match p.slice() {
          "u" => {
            p.bump(); // eat `u`

            // Eat the number.
            //
            // FIXME: This is wrong if the number literal is too long. Need to change the
            // tokenizer to not parse numbers.
            let mut chars = 0;
            while chars < 4 {
              chars += p.slice().len();
              p.bump();
            }
          }
          "\"" | "'" | "\\" | "n" | "r" | "t" | "b" | "f" => {
            p.bump(); // eat the escape
          }
          _ => {
            // When there are interpolations, escapes can be whatever.
            if !interpolations {
              p.error("unknown escape code");
            }
            // Assume a single letter.
            p.bump();
          }
        }
      }

      _ => {
        p.bump();
      }
    }

    is_start = false;
  }

  // This is dumb, but changing the parser mode is difficult with tripple quoted
  // strings, so this is the easiest way to handle it.
  while p.at(WHITESPACE) {
    p.bump();
  }
}

// test ok
// if (true) 3
fn if_expr(p: &mut Parser) {
  p.eat(T![if]);

  p.expect(T!['(']);
  // test ok
  // if (
  //   true
  // ) println(3)
  p.eat_newlines();
  expr(p);
  p.eat_newlines();
  p.expect(T![')']);

  // test ok
  // if (true)
  //   println("hi")
  p.eat_newlines();

  expr(p);

  // Eat enough newlines so we can check for an `else`.
  while p.at(T![nl]) && p.peek() == T![nl] {
    p.eat(T![nl]);
  }

  // test ok
  // if (true) 3
  // else 4
  if p.at(T![nl]) && p.peek() == T![else] {
    p.eat(T![nl]);
  }

  // test ok
  // if (true) 3 else 4
  if p.at(T![else]) {
    p.eat(T![else]);
    // test ok
    // if (true) 3 else
    // 4
    p.eat_newlines();
    expr(p);
  }
}

// test ok
// while (true) 3
fn while_expr(p: &mut Parser) {
  p.eat(T![while]);

  p.expect(T!['(']);
  // test ok
  // if (
  //   true
  // ) println(3)
  p.eat_newlines();
  expr(p);
  p.eat_newlines();
  p.expect(T![')']);

  // test ok
  // while (true)
  //   println("hi")
  if p.at(T![nl]) {
    p.eat(T![nl]);
  }

  expr(p);
}

// test ok
// do println("hi") while (true)
fn do_while_expr(p: &mut Parser) {
  p.eat(T![do]);

  expr(p);

  p.expect(T![while]);

  expr(p);
}

// test ok
// for (x <- 1 to 10) println(x)
fn for_expr(p: &mut Parser) {
  p.eat(T![for]);

  let end = if p.at(T!['(']) {
    p.eat(T!['(']);
    T![')']
  } else if p.at(T!['{']) {
    p.eat(T!['{']);
    T!['}']
  } else {
    p.error("expected `(` or `{`");
    return;
  };

  p.eat_newlines();

  loop {
    generator(p);

    let found_newline = p.at(T![nl]);
    p.eat_newlines();

    if p.at(end) {
      p.eat(end);
      break;
    }
    if p.at(EOF) {
      p.error("unexpected end of file");
      break;
    }

    if !found_newline {
      p.error("expected newline");
      p.recover_until_any(&[T![nl], T![')'], T!['}']]);
      break;
    }
  }

  // test ok
  // for (x <- 1 to 10)
  //   yield x
  p.eat_newlines();

  if p.at(T![yield]) {
    p.eat(T![yield]);
  }

  // test ok
  // for (x <- 1 to 10) yield
  //   x
  p.eat_newlines();

  expr(p);
}

// Part of a for, like `pat <- expr` or `pat = expr`.
fn generator(p: &mut Parser) {
  let m = p.start();

  super::pattern::pattern_val(p);

  // test ok
  // for (
  //   x
  //     <- 1 to 10
  // ) yield x
  p.eat_newlines();

  let generator = match p.current() {
    T![<-] => {
      p.eat(T![<-]);
      FLATMAP_GENERATOR
    }

    T![=] => {
      p.eat(T![=]);
      ASSIGNMENT_GENERATOR
    }

    _ => {
      p.error("expected `<-` or `=`");
      m.abandon(p);
      return;
    }
  };

  // test ok
  // for (
  //   x <-
  //     1 to 10
  // ) yield x
  p.eat_newlines();

  expr(p);

  // This is allowed, for some reason.
  //
  // test ok
  // for {
  //   x <- 1 to 10
  //   foo = 3,
  // } yield x
  if p.at(T![,]) && p.peek() == T![nl] {
    p.eat(T![,]);
  }

  m.complete(p, generator);

  // test ok
  // for {
  //   x <- 1 to 10
  //   if x % 2 == 2
  // } yield x
  // for {
  //   x <- 1 to 10 if x % 2 == 2
  // } yield x
  if p.at(T![if]) || (p.at(T![nl]) && p.peek() == T![if]) {
    let m = p.start();
    p.eat_newlines();
    p.eat(T![if]);
    super::expr::expr_no_fat_arrow(p);
    m.complete(p, GUARD_GENERATOR);
  }
}

// test ok
// try file.read() catch {
//   case _ => println(x)
// }
fn try_expr(p: &mut Parser) {
  p.eat(T![try]);

  expr(p);

  p.eat_newlines();

  // test ok
  // try file.read() catch {
  //   file.close()
  // }
  if p.at(T![catch]) || (p.at(T![nl]) && p.peek() == T![finally]) {
    p.eat_newlines();
    p.eat(T![catch]);

    expr(p);
  }

  // test ok
  // try file.read() finally {
  //   file.close()
  // }
  // try file.read()
  // finally file.close()
  if p.at(T![finally]) || (p.at(T![nl]) && p.peek() == T![finally]) {
    p.eat_newlines();
    p.eat(T![finally]);

    expr(p);
  }
}

// test ok
// macro foo.bar.Baz
fn macro_expr(p: &mut Parser) {
  p.eat(T![macro]);

  let m = p.start();
  p.expect(T![ident]);

  while p.at(T![.]) {
    p.eat(T![.]);
    p.expect(T![ident]);
  }

  m.complete(p, PATH);
}
