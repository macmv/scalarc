use crate::{
  syntax_kind::{SyntaxKind, SyntaxKind::*, T},
  Parser,
};

pub fn type_expr(p: &mut Parser) { type_expr_0(p, false); }
pub fn type_expr_is_case(p: &mut Parser, is_case: bool) { type_expr_0(p, is_case); }

fn type_expr_0(p: &mut Parser, is_case: bool) {
  let mut m = p.start();
  if p.at(T![=>]) && !is_case {
    // test ok
    // def foo: => Int = 0
    p.eat(T![=>]);
    let prefix = m.complete(p, ANON_LAMBDA_TYPE);
    m = prefix.precede(p)
  }

  let mut lhs = match p.current() {
    // test ok
    // val foo: () = 0
    // val foo: (Int, String) = 0
    T!['('] => {
      type_args_0(p, T!['('], T![')'], true);
      m.complete(p, TUPLE_TYPE)
    }

    T![ident] => {
      p.eat(T![ident]);
      m.complete(p, SIMPLE_TYPE)
    }

    _ => {
      p.error("expected type");
      m.abandon(p);

      return;
    }
  };

  loop {
    match p.current() {
      T![.] => {
        let m = lhs.precede(p);
        p.eat(T![.]);
        p.expect(T![ident]);
        lhs = m.complete(p, PATH_TYPE);
      }

      T!['['] => {
        let m = lhs.precede(p);
        {
          let m = p.start();
          type_args(p, T!['['], T![']']);
          m.complete(p, TYPE_ARGS);
        }
        lhs = m.complete(p, GENERIC_TYPE);
      }

      T![=>] => {
        if is_case {
          // This is not a lambda, its a pattern guard.
          return;
        } else {
          let m = lhs.precede(p);
          p.eat(T![=>]);
          type_expr(p);
          lhs = m.complete(p, LAMBDA_TYPE);
        }
      }

      T![ident] if p.slice() == "*" => {
        // test ok
        // def foo(x: Int*) = 0
        let m = lhs.precede(p);
        p.eat(T![ident]);
        m.complete(p, SPREAD_TYPE);
        return;
      }

      T![,] | T![']'] | T![')'] | T!['}'] | T![=] | T![nl] | T![<:] | T![>:] | T![:] | EOF => {
        return
      }

      // This is for class definitions. Not sure if correct or not.
      T!['{'] | T![with] => return,

      // test ok
      // case _: Int | _: String =>
      T![ident] if p.slice() == "|" => return,

      _ => {
        p.error(format!("expected type, got {:?}", p.current()));
        p.recover_until_any(&[T![nl], T![,], T![')'], T!['}'], T![=]]);
        return;
      }
    }
  }
}

// A type parameter on a definition, like `A <: B`.
pub fn type_param(p: &mut Parser) {
  let mut m = p.start();

  if p.at(T![ident]) && p.slice() == "+" {
    // test ok
    // def foo[+A] = 3
    p.eat(T![ident]);
    type_expr(p);
    let c = m.complete(p, COVARIANT_PARAM);
    m = c.precede(p);
  } else if p.at(T![ident]) && p.slice() == "-" {
    // test ok
    // def foo[-A] = 3
    p.eat(T![ident]);
    type_expr(p);
    let c = m.complete(p, CONTRAVARIANT_PARAM);
    m = c.precede(p);
  } else {
    // test ok
    // def foo[A] = 3
    type_expr(p);
  }

  if p.at(T![<:]) {
    // test ok
    // def foo[T <: Int] = 3
    p.eat(T![<:]);
    type_expr(p);
    m.complete(p, LOWER_BOUND_PARAM);
  } else if p.at(T![>:]) {
    // test ok
    // def foo[T >: Int] = 3
    p.eat(T![>:]);
    type_expr(p);
    m.complete(p, UPPER_BOUND_PARAM);
  } else if p.at(T![:]) {
    // test ok
    // def foo[T: Int] = 3
    p.eat(T![:]);
    type_expr(p);
    m.complete(p, IMPLICIT_PARAM);
  } else {
    m.complete(p, SIMPLE_PARAM);
  }
}

/// Type arguments. These show up in function calls, like `foo[Int, String]`.
pub fn type_args(p: &mut Parser, start: SyntaxKind, end: SyntaxKind) {
  type_args_0(p, start, end, false);
}

fn type_args_0(p: &mut Parser, start: SyntaxKind, end: SyntaxKind, allow_empty: bool) {
  p.eat(start);

  if allow_empty && p.at(end) {
    p.eat(end);
    return;
  }

  p.eat_newlines();
  loop {
    type_expr(p);
    p.eat_newlines();

    // test ok
    // Seq.empty[Int, String]
    if p.current() == T![,] {
      p.eat(T![,]);

      // test ok
      // Seq.empty[
      //   Int
      //   ,
      //   Int
      // ]
      p.eat_newlines();
    } else {
      p.expect(end);
      break;
    }
  }
}

/// Type parameters. These show up in function definitions, like `def foo[A <:
/// B]`.
pub fn type_params(p: &mut Parser, start: SyntaxKind, end: SyntaxKind) {
  p.eat(start);

  p.eat_newlines();
  loop {
    type_param(p);
    p.eat_newlines();

    // test ok
    // val f: Foo[Int, String] = 3
    if p.current() == T![,] {
      p.eat(T![,]);

      // test ok
      // def foo(
      //   a: Int
      //   ,
      //   b: Int
      // ) = 3
      p.eat_newlines();
    } else {
      p.expect(end);
      break;
    }
  }
}
