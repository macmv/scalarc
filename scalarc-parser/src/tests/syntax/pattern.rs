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
          PATH_PATTERN
            PATH
              IDENT 'Nil'
          WHITESPACE ' '
          FAT_ARROW '=>'
          WHITESPACE ' '
          BLOCK
            EXPR_ITEM
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
            PATH
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
          BLOCK
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '1'
    "#],
  );

  check(
    "case 1 | Seq(1, 2) => 1",
    expect![@r#"
      SOURCE_FILE
        CASE_ITEM
          CASE_KW 'case'
          WHITESPACE ' '
          UNION_PATTERN
            LIT_PATTERN
              INT_LIT_KW '1'
            WHITESPACE ' '
            IDENT '|'
            WHITESPACE ' '
            ARG_PATTERN
              PATH
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
          BLOCK
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '1'
    "#],
  );

  check(
    "case 1 | 2 | 3 => 1",
    expect![@r#"
      SOURCE_FILE
        CASE_ITEM
          CASE_KW 'case'
          WHITESPACE ' '
          UNION_PATTERN
            UNION_PATTERN
              LIT_PATTERN
                INT_LIT_KW '1'
              WHITESPACE ' '
              IDENT '|'
              WHITESPACE ' '
              LIT_PATTERN
                INT_LIT_KW '2'
            WHITESPACE ' '
            IDENT '|'
            WHITESPACE ' '
            LIT_PATTERN
              INT_LIT_KW '3'
          WHITESPACE ' '
          FAT_ARROW '=>'
          WHITESPACE ' '
          BLOCK
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '1'
    "#],
  );

  check(
    "case foo: Int => 1",
    expect![@r#"
      SOURCE_FILE
        CASE_ITEM
          CASE_KW 'case'
          WHITESPACE ' '
          TYPE_PATTERN
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            SIMPLE_TYPE
              IDENT 'Int'
          WHITESPACE ' '
          FAT_ARROW '=>'
          WHITESPACE ' '
          BLOCK
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '1'
    "#],
  );

  check(
    "case list @ Seq(1, 2) => 1",
    expect![@r#"
      SOURCE_FILE
        CASE_ITEM
          CASE_KW 'case'
          WHITESPACE ' '
          AT_PATTERN
            IDENT 'list'
            WHITESPACE ' '
            IDENT '@'
            WHITESPACE ' '
            ARG_PATTERN
              PATH
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
          BLOCK
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '1'
    "#],
  );
}

#[test]
fn paths_work() {
  check(
    "case foo.bar.baz => 1",
    expect![@r#"
      SOURCE_FILE
        CASE_ITEM
          CASE_KW 'case'
          WHITESPACE ' '
          PATH_PATTERN
            PATH
              IDENT 'foo'
              DOT '.'
              IDENT 'bar'
              DOT '.'
              IDENT 'baz'
          WHITESPACE ' '
          FAT_ARROW '=>'
          WHITESPACE ' '
          BLOCK
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '1'
    "#],
  );

  check(
    "case scala.Seq(1, 2) => 1",
    expect![@r#"
      SOURCE_FILE
        CASE_ITEM
          CASE_KW 'case'
          WHITESPACE ' '
          ARG_PATTERN
            PATH
              IDENT 'scala'
              DOT '.'
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
          BLOCK
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '1'
    "#],
  );
}
