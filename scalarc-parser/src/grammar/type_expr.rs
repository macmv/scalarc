use crate::{
  syntax_kind::{
    SyntaxKind::{self, *},
    T,
  },
  CompletedMarker, Parser,
};

pub fn simple_type_expr(p: &mut Parser) -> Option<CompletedMarker> {
  simple_type_expr_0(p, false, false, false)
}

// test ok
// x match {
//   case foo: Int @ unchecked =>
// }
pub fn simple_type_expr_no_arrow(p: &mut Parser) -> Option<CompletedMarker> {
  simple_type_expr_0(p, true, false, false)
}
pub fn simple_type_expr_is_case(p: &mut Parser, is_case: bool) {
  simple_type_expr_0(p, is_case, true, false);
}

fn simple_type_expr_0(
  p: &mut Parser,
  is_case: bool,
  allow_with: bool,
  is_nested_params: bool,
) -> Option<CompletedMarker> {
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

    // test ok
    // def foo: true = true
    T![true] | T![false] => {
      p.bump();
      m.complete(p, LIT_TYPE)
    }

    // I cannot figure out the grammar for this, so we're guessing here.
    // test ok
    // val foo: ({ type T = Int }) = 3
    T!['{'] => {
      p.eat(T!['{']);
      loop {
        let m = p.start();
        match p.current() {
          // test ok
          // foo.asInstanceOf[{ def bar(): Int }]
          T![def] => super::item::fun_def(p, m),
          T![val] | T![var] => super::item::val_def(p, m),
          T![type] => super::item::type_def(p, m),

          T!['}'] => {
            m.abandon(p);
            p.eat(T!['}']);
            break;
          }
          _ => {
            m.abandon(p);
            p.error("expected type definition");
            p.recover_until(T!['}']);
          }
        }
      }

      m.complete(p, BLOCK_TYPE)
    }

    _ => {
      p.error("expected type");
      m.abandon(p);

      return None;
    }
  };

  loop {
    match p.current() {
      T![.] => {
        let m = lhs.precede(p);
        p.eat(T![.]);

        match p.current() {
          // test ok
          // val foo: Bar.Baz = 3
          // val bar: this.type = 3
          T![ident] | T![type] => {
            p.bump();
          }
          _ => p.error("expected identifier"),
        };

        lhs = m.complete(p, PATH_TYPE);
      }

      T![#] => {
        let m = lhs.precede(p);
        p.eat(T![#]);

        match p.current() {
          // test ok
          // val foo: Foo#bar = 3
          T![ident] | T![type] => {
            p.bump();
          }
          _ => p.error("expected identifier"),
        };

        lhs = m.complete(p, NESTED_TYPE);
      }

      T!['['] => {
        let m = lhs.precede(p);
        {
          let m = p.start();
          if is_nested_params {
            type_params(p, T!['['], T![']']);
          } else {
            type_args(p, T!['['], T![']']);
          }
          m.complete(p, TYPE_ARGS);
        }
        lhs = m.complete(p, GENERIC_TYPE);
      }

      T![=>] => {
        if is_case {
          // This is not a lambda, its a pattern guard.
          return Some(lhs);
        } else {
          let m = lhs.precede(p);
          p.eat(T![=>]);
          simple_type_expr_0(p, is_case, allow_with, is_nested_params);
          lhs = m.complete(p, LAMBDA_TYPE);
        }
      }

      // test ok
      // val foo: Int with String = 5
      T![with] if allow_with => {
        let m = lhs.precede(p);
        p.eat(T![with]);
        simple_type_expr_0(p, is_case, allow_with, is_nested_params);
        lhs = m.complete(p, WITH_TYPE);
      }

      T![ident] if p.slice() == "*" => {
        // test ok
        // def foo(x: Int*) = 0
        let m = lhs.precede(p);
        p.eat(T![ident]);
        lhs = m.complete(p, SPREAD_TYPE);
        return Some(lhs);
      }

      T![@] => {
        // test ok
        // (x: Int @unchecked) match {
        //   case 1 => 2
        // }
        super::item::annotation(p);
      }

      // test ok
      // case v: Int if v > 0 => v
      // case _: Int | _: String =>
      _ => return Some(lhs),
    }
  }
}

// A type parameter on a definition, like `A <: B`.
pub fn type_expr(p: &mut Parser) -> Option<CompletedMarker> { type_expr_0(p, false, false) }
pub fn type_expr_is_case(p: &mut Parser) -> Option<CompletedMarker> { type_expr_0(p, true, false) }

fn type_expr_0(p: &mut Parser, is_case: bool, is_nested_params: bool) -> Option<CompletedMarker> {
  // test ok
  // type Foo = Bar with Seq[Int]
  let mut lhs = simple_type_expr_0(p, is_case, true, is_nested_params)?;

  loop {
    match p.current() {
      T![<:] => {
        // test ok
        // def foo[T <: Int] = 3
        let m = lhs.precede(p);
        p.eat(T![<:]);
        // test ok
        // trait Foo[+C <: Set with SetOps[C]]
        type_expr(p);
        lhs = m.complete(p, LOWER_BOUND_TYPE);
      }
      T![>:] => {
        // test ok
        // def foo[T >: Int] = 3
        let m = lhs.precede(p);
        p.eat(T![>:]);
        type_expr(p);
        lhs = m.complete(p, UPPER_BOUND_TYPE);
      }

      // test ok
      // def foo: Foo { type T = Int } = 3
      T!['{'] => {
        let m = lhs.precede(p);
        p.eat(T!['{']);
        loop {
          // test ok
          // def foo: Foo {
          //   type T = Int
          // } = 3
          p.eat_newlines();
          let m = p.start();
          match p.current() {
            T![def] => super::item::fun_def(p, m),
            T![val] | T![var] => super::item::val_def(p, m),
            T![type] => super::item::type_def(p, m),

            T!['}'] => {
              m.abandon(p);
              p.eat(T!['}']);
              break;
            }
            _ => {
              m.abandon(p);
              p.error("expected type definition");
              p.recover_until(T!['}']);
            }
          }
        }

        lhs = m.complete(p, REFINED_TYPE)
      }

      T![ident] if !is_case => {
        // test ok
        // def foo[T](implicit ev: V#T <:< T): T = 3
        let m = lhs.precede(p);
        p.eat(T![ident]);
        type_expr(p);
        lhs = m.complete(p, INFIX_TYPE);
      }

      _ => break Some(lhs),
    }
  }
}

// A type parameter on a definition, like `A <: B: C`.
pub fn type_param(p: &mut Parser) -> Option<CompletedMarker> {
  // test ok
  // def foo[@foo +A] = 3
  while p.at(T![@]) {
    super::item::annotation(p);
  }

  let mut c = if p.at(T![ident]) && p.slice() == "+" {
    // test ok
    // def foo[+A] = 3
    let m = p.start();
    p.eat(T![ident]);
    type_expr_0(p, false, true);
    m.complete(p, COVARIANT_PARAM)
  } else if p.at(T![ident]) && p.slice() == "-" {
    // test ok
    // def foo[-A] = 3
    let m = p.start();
    p.eat(T![ident]);
    type_expr_0(p, false, true);
    m.complete(p, CONTRAVARIANT_PARAM)
  } else {
    // test ok
    // def foo[A] = 3
    type_expr_0(p, false, true)?
  };

  while p.at(T![:]) {
    // test ok
    // def foo[T: Int: String] = 3
    let m = c.precede(p);
    p.eat(T![:]);
    type_expr(p);
    c = m.complete(p, IMPLICIT_PARAM);
  }

  Some(c)
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

/// Type parameters. These show up where generics are created, like in
/// `def foo[A <: B: C]`.
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
