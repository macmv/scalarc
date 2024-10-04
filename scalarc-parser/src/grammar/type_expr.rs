use crate::{
  syntax_kind::{SyntaxKind, SyntaxKind::*, T},
  Parser,
};

pub fn type_expr(p: &mut Parser) { type_expr_0(p, false); }
pub fn type_expr_pattern(p: &mut Parser) { type_expr_0(p, true); }

fn type_expr_0(p: &mut Parser, pattern: bool) {
  let m = p.start();
  let mut lhs = match p.current() {
    // test ok
    // val foo: (Int, String) = 0
    T!['('] => {
      type_params(p, T!['('], T![')']);
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
          type_params(p, T!['['], T![']']);
          m.complete(p, TYPE_PARAMS);
        }
        lhs = m.complete(p, GENERIC_TYPE);
      }

      T![=>] => {
        if pattern {
          // This is not a lambda, its a pattern guard.
          return;
        } else {
          let m = lhs.precede(p);
          p.eat(T![=>]);
          type_expr(p);
          lhs = m.complete(p, LAMBDA_TYPE);
        }
      }

      T![,] | T![']'] | T![')'] | T!['}'] | T![=] | T![nl] | T![<:] | T![>:] | T![:] | EOF => {
        return
      }

      // This is for class definitions. Not sure if correct or not.
      T!['{'] | T![with] => return,

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
  let m = p.start();
  type_expr(p);

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

pub fn type_args(p: &mut Parser) {
  let m = p.start();
  p.eat(T!['[']);

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
      p.expect(T![']']);
      m.complete(p, TYPE_ARGS);
      break;
    }
  }
}

pub fn type_params(p: &mut Parser, start: SyntaxKind, end: SyntaxKind) {
  p.eat(start);

  p.eat_newlines();
  loop {
    type_expr(p);
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
