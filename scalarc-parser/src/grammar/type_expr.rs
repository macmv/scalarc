use crate::{
  syntax_kind::{SyntaxKind::*, T},
  Parser,
};

pub fn type_expr(p: &mut Parser) {
  let mut m = p.start();

  loop {
    p.expect(T![ident]);

    p.eat_newlines();

    match p.current() {
      T![.] => {
        let lhs = m.complete(p, TYPE_ARGS);
        m = lhs.precede(p);
      }
      T!['['] => {
        p.eat(T!['[']);
        p.eat_newlines();
        type_expr(p);
        p.eat_newlines();
        p.expect(T![']']);
      }

      T![,] | T![']'] | T![')'] | T!['}'] | T![=] => {
        m.complete(p, TYPE_ARGS);
        return;
      }

      _ => {
        m.abandon(p);
        p.error(format!("expected type, got {:?}", p.current()));
        p.recover_until_any(&[T![nl], T![,], T![')'], T!['}'], T![=]]);
        return;
      }
    }
  }
}
