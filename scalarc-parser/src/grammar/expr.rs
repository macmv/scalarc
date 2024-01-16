use super::*;

pub fn expr(p: &mut Parser) {
  let m = p.start();

  simple_expr(p);

  match p.current() {
    T![ident] => {
      p.eat(T![ident]);
      expr(p);
      m.complete(p, EXPR);
    }

    T![nl] => {
      m.complete(p, EXPR);
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
    LITERAL => {
      p.eat(LITERAL);
      m.complete(p, SIMPLE_EXPR);
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
}
