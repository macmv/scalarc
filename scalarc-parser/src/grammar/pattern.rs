use crate::CompletedMarker;

use super::*;

pub fn pattern(p: &mut Parser) { pattern_inner(p); }

fn pattern_inner(p: &mut Parser) -> Option<CompletedMarker> {
  let _ = atom_pattern(p)?;

  None
}

// Paterns like `Seq(1)` or `Nil`
fn atom_pattern(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  match p.current() {
    INT_LIT_KW => {
      p.eat(INT_LIT_KW);
      Some(m.complete(p, LIT_PATTERN))
    }
    STRING_LIT_KW => {
      p.eat(STRING_LIT_KW);
      Some(m.complete(p, LIT_PATTERN))
    }
    IDENT => {
      p.eat(IDENT);

      if p.current() == T!['('] {
        arg_pattern(p);
        Some(m.complete(p, ARG_PATTERN))
      } else {
        Some(m.complete(p, IDENT_PATTERN))
      }
    }

    _ => {
      m.abandon(p);
      p.error(format!("expected pattern, got {:?}", p.current()));
      p.recover_until(T![nl]);
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

#[cfg(test)]
mod tests {
  use crate::tests::check;

  #[test]
  fn patterns() {
    check(
      "case Nil => 1",
      expect![@r#"
        SOURCE_FILE
          CASE_ITEM
            CASE_KW 'case'
            WHITESPACE ' '
            IDENT_PATTERN
              IDENT 'Nil'
            WHITESPACE ' '
            FAT_ARROW '=>'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '1'
      "#],
    );

    check(
      "case Seq(1, 2) => 1",
      expect![@r#"
        SOURCE_FILE
          CASE_ITEM
            CASE_KW 'case'
            WHITESPACE ' '
            ARG_PATTERN
              IDENT 'Seq'
              PATTERN_ARGS
                OPEN_PAREN '('
                LIT_PATTERN
                  INT_LIT_KW '1'
                COMMA ','
                WHITESPACE ' '
                LIT_PATTERN
                  INT_LIT_KW '2'
                CLOSE_PAREN ')'
            WHITESPACE ' '
            FAT_ARROW '=>'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '1'
      "#],
    );
  }
}
