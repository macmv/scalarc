use super::*;

pub fn pattern(p: &mut Parser) {
  let m = p.start();
  p.expect(T![ident]);
  m.complete(p, IDENT_PATTERN);
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
            IDENT_PATTERN
              IDENT 'Seq'
            error: expected FAT_ARROW
            error: expected expression, got OPEN_PAREN
            OPEN_PAREN '('
            INT_LIT_KW '1'
            COMMA ','
            WHITESPACE ' '
            INT_LIT_KW '2'
            CLOSE_PAREN ')'
            WHITESPACE ' '
            FAT_ARROW '=>'
            WHITESPACE ' '
            INT_LIT_KW '1'
      "#],
    );
  }
}
