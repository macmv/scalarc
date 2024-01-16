use super::*;

pub fn expr(p: &mut Parser) {
  let m = p.start();

  simple_expr(p);

  match p.current() {
    T![ident] => {
      p.eat(T![ident]);
      expr(p);
      m.complete(p, INFIX_EXPR);
    }

    T![nl] => {
      m.abandon(p);
      p.eat(T![nl]);
    }

    _ => {
      m.abandon(p);
      p.error(format!("expected expression, got {:?}", p.current()));
      p.recover_until(T![nl]);
    }
  }
}

fn simple_expr(p: &mut Parser) {
  let m = p.start();

  match p.current() {
    INT_LIT_KW => {
      p.eat(INT_LIT_KW);
      m.complete(p, LITERAL);
    }

    _ => {
      m.abandon(p);
      p.error(format!("expected expression, got {:?}", p.current()));
      p.recover_until(T![nl]);
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::tests::check_expr;

  #[test]
  fn literals() {
    check_expr("2", r"LITERAL");

    // TODO
    // check_expr("\"hi\"", r"LITERAL");
  }

  #[test]
  fn binary_op() {
    check_expr(
      "1 + 2",
      r"EXPR
          SIMPLE_EXPR
            LITERAL
          IDENT
          EXPR
            SIMPLE_EXPR
              LITERAL
          NL_KW",
    );
  }

  #[test]
  fn complex_op() {
    check_expr(
      "2 + 2",
      r"INFIX_EXPR
          LITERAL
            INT_LIT_KW '2'
          IDENT '+'
          LITERAL
            INT_LIT_KW '2'
          NL_KW",
    );
    /*
    check_expr(
      "2 + 2 == 5",
      r"INFIX_EXPR
          INFIX_EXPR
            LITERAL
              INT_NUMBER '2'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LITERAL
              INT_NUMBER '2'
          WHITESPACE ' '
          EQ2 '=='
          WHITESPACE ' '
          LITERAL
            INT_NUMBER '5'",
    );
    */
  }
}
