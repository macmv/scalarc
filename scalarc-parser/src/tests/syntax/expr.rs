use crate::tests::{check, check_expr};

#[test]
fn literals() {
  check_expr(
    "2",
    expect![@r#"
      LIT_EXPR
        INT_LIT_KW '2'
    "#],
  );

  check_expr(
    "true + false",
    expect![@r#"
      INFIX_EXPR
        LIT_EXPR
          TRUE_KW 'true'
        WHITESPACE ' '
        IDENT '+'
        WHITESPACE ' '
        LIT_EXPR
          FALSE_KW 'false'
    "#],
  );

  check_expr(
    "\"hi\"",
    expect![@r#"
      DOUBLE_QUOTED_STRING
        DOUBLE_QUOTE '"'
        IDENT 'hi'
        DOUBLE_QUOTE '"'
    "#],
  );

  check_expr(
    "\"foo \\\" bar {{\"",
    expect![@r#"
      DOUBLE_QUOTED_STRING
        DOUBLE_QUOTE '"'
        IDENT 'foo'
        WHITESPACE ' '
        IDENT '\'
        DOUBLE_QUOTE '"'
        WHITESPACE ' '
        IDENT 'bar'
        WHITESPACE ' '
        OPEN_CURLY '{'
        OPEN_CURLY '{'
        DOUBLE_QUOTE '"'
    "#],
  );

  check_expr(
    "'a'",
    expect![@r#"
      CHARACTER_LIT
        SINGLE_QUOTE '''
        IDENT 'a'
        SINGLE_QUOTE '''
    "#],
  );
}

#[test]
fn whitespace() {
  check_expr(
    "2   +  56",
    expect![@r#"
      INFIX_EXPR
        LIT_EXPR
          INT_LIT_KW '2'
        WHITESPACE '   '
        IDENT '+'
        WHITESPACE '  '
        LIT_EXPR
          INT_LIT_KW '56'
    "#],
  );

  check_expr(
    "2 +\n 3",
    expect![@r#"
      INFIX_EXPR
        LIT_EXPR
          INT_LIT_KW '2'
        WHITESPACE ' '
        IDENT '+'
        NL_KW '\n'
        WHITESPACE ' '
        LIT_EXPR
          INT_LIT_KW '3'
    "#],
  );

  check_expr(
    "2 +\n\n 3",
    expect![@r#"
      LIT_EXPR
        INT_LIT_KW '2'
      WHITESPACE ' '
      IDENT '+'
      NL_KW '\n'
      NL_KW '\n'
      error: expected expression, got expression separator INT_LIT_KW
      WHITESPACE ' '
      INT_LIT_KW '3'
    "#],
  );
}

#[test]
fn binary_op() {
  check_expr(
    "1 + 2",
    expect![@r#"
      INFIX_EXPR
        LIT_EXPR
          INT_LIT_KW '1'
        WHITESPACE ' '
        IDENT '+'
        WHITESPACE ' '
        LIT_EXPR
          INT_LIT_KW '2'
    "#],
  );

  check_expr(
    "\"hi\" + \"there\"",
    expect![@r#"
      INFIX_EXPR
        DOUBLE_QUOTED_STRING
          DOUBLE_QUOTE '"'
          IDENT 'hi'
          DOUBLE_QUOTE '"'
        WHITESPACE ' '
        IDENT '+'
        WHITESPACE ' '
        DOUBLE_QUOTED_STRING
          DOUBLE_QUOTE '"'
          IDENT 'there'
          DOUBLE_QUOTE '"'
    "#],
  );
}

#[test]
fn complex_op() {
  check_expr(
    "2 + 2",
    expect![@r#"
      INFIX_EXPR
        LIT_EXPR
          INT_LIT_KW '2'
        WHITESPACE ' '
        IDENT '+'
        WHITESPACE ' '
        LIT_EXPR
          INT_LIT_KW '2'
    "#],
  );

  check_expr(
    "2 + 2 == 5",
    expect![@r#"
      INFIX_EXPR
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '2'
        WHITESPACE ' '
        IDENT '=='
        WHITESPACE ' '
        LIT_EXPR
          INT_LIT_KW '5'
    "#],
  );

  check_expr(
    "2 == 2 + 5",
    expect![@r#"
      INFIX_EXPR
        LIT_EXPR
          INT_LIT_KW '2'
        WHITESPACE ' '
        IDENT '=='
        WHITESPACE ' '
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '5'
    "#],
  );
}

#[test]
fn associativity() {
  check_expr(
    "2 + 3 + 4",
    expect![@r#"
      INFIX_EXPR
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '2'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
        WHITESPACE ' '
        IDENT '+'
        WHITESPACE ' '
        LIT_EXPR
          INT_LIT_KW '4'
    "#],
  );

  // All ops ending in `:` are right-associative
  check_expr(
    "2 +: 3 +: 4",
    expect![@r#"
      INFIX_EXPR
        LIT_EXPR
          INT_LIT_KW '2'
        WHITESPACE ' '
        IDENT '+:'
        WHITESPACE ' '
        INFIX_EXPR
          LIT_EXPR
            INT_LIT_KW '3'
          WHITESPACE ' '
          IDENT '+:'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '4'
    "#],
  );
}

#[test]
fn lambda_expr() {
  // This might be a bit difficult to parse up the chain. Ah well, this way avoids
  // special cases in the parser, which is nice.
  check_expr(
    "x => x + 3",
    expect![@r#"
      INFIX_EXPR
        IDENT_EXPR
          IDENT 'x'
        WHITESPACE ' '
        FAT_ARROW '=>'
        WHITESPACE ' '
        INFIX_EXPR
          IDENT_EXPR
            IDENT 'x'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  // FIXME: Type annotations are valid anywhere, so we should be able to parse
  // `x: Int` as an expression.
  check_expr(
    "(x: Int) => x + 3",
    expect![@r#"
      INFIX_EXPR
        TUPLE_EXPR
          OPEN_PAREN '('
          IDENT_EXPR
            IDENT 'x'
          error: expected operator, got COLON
          COLON ':'
          WHITESPACE ' '
          IDENT 'Int'
          CLOSE_PAREN ')'
        WHITESPACE ' '
        FAT_ARROW '=>'
        WHITESPACE ' '
        INFIX_EXPR
          IDENT_EXPR
            IDENT 'x'
          WHITESPACE ' '
          IDENT '+'
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );
}

#[test]
fn call_op() {
  check_expr(
    "hi(2)",
    expect![@r#"
      CALL_EXPR
        IDENT_EXPR
          IDENT 'hi'
        PAREN_ARGUMENTS
          OPEN_PAREN '('
          LIT_EXPR
            INT_LIT_KW '2'
          CLOSE_PAREN ')'
    "#],
  );

  check_expr(
    "hi(2, 3)",
    expect![@r#"
      CALL_EXPR
        IDENT_EXPR
          IDENT 'hi'
        PAREN_ARGUMENTS
          OPEN_PAREN '('
          LIT_EXPR
            INT_LIT_KW '2'
          COMMA ','
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
          CLOSE_PAREN ')'
    "#],
  );

  check_expr(
    "hi { 3 }",
    expect![@r#"
      CALL_EXPR
        IDENT_EXPR
          IDENT 'hi'
        WHITESPACE ' '
        BLOCK_EXPR
          OPEN_CURLY '{'
          WHITESPACE ' '
          EXPR_ITEM
            LIT_EXPR
              INT_LIT_KW '3'
          WHITESPACE ' '
          CLOSE_CURLY '}'
    "#],
  );

  check_expr(
    "hi(foo = 3, bar = 4)",
    expect![@r#"
      CALL_EXPR
        IDENT_EXPR
          IDENT 'hi'
        PAREN_ARGUMENTS
          OPEN_PAREN '('
          IDENT 'foo'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
          COMMA ','
          WHITESPACE ' '
          IDENT 'bar'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '4'
          CLOSE_PAREN ')'
    "#],
  );

  check_expr(
    "hi(foo)",
    expect![@r#"
      CALL_EXPR
        IDENT_EXPR
          IDENT 'hi'
        PAREN_ARGUMENTS
          OPEN_PAREN '('
          IDENT_EXPR
            IDENT 'foo'
          CLOSE_PAREN ')'
    "#],
  );
}

#[test]
fn dot_exprs() {
  check_expr(
    "foo.bar",
    expect![@r#"
      FIELD_EXPR
        IDENT_EXPR
          IDENT 'foo'
        DOT '.'
        IDENT 'bar'
    "#],
  );

  check_expr(
    "foo.bar.baz",
    expect![@r#"
      FIELD_EXPR
        FIELD_EXPR
          IDENT_EXPR
            IDENT 'foo'
          DOT '.'
          IDENT 'bar'
        DOT '.'
        IDENT 'baz'
    "#],
  );

  check_expr(
    "foo.3",
    expect![@r#"
      FIELD_EXPR
        IDENT_EXPR
          IDENT 'foo'
        DOT '.'
        error: expected identifier, got INT_LIT_KW
      error: expected operator, got INT_LIT_KW
      INT_LIT_KW '3'
    "#],
  );

  check_expr(
    "foo.",
    expect![@r#"
      FIELD_EXPR
        IDENT_EXPR
          IDENT 'foo'
        DOT '.'
        error: expected identifier, got EOF
    "#],
  );
}

#[test]
fn block_expr() {
  check_expr(
    "{ 2 + 3 }",
    expect![@r#"
      BLOCK_EXPR
        OPEN_CURLY '{'
        WHITESPACE ' '
        EXPR_ITEM
          INFIX_EXPR
            LIT_EXPR
              INT_LIT_KW '2'
            WHITESPACE ' '
            IDENT '+'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
        WHITESPACE ' '
        CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn newline_magic() {
  check(
    "println {
       3
     }",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          CALL_EXPR
            IDENT_EXPR
              IDENT 'println'
            WHITESPACE ' '
            BLOCK_EXPR
              OPEN_CURLY '{'
              NL_KW '\n'
              WHITESPACE '       '
              EXPR_ITEM
                LIT_EXPR
                  INT_LIT_KW '3'
              NL_KW '\n'
              WHITESPACE '     '
              CLOSE_CURLY '}'
    "#],
  );

  // TODO: This should parse the same as the above
  check(
    "println
     {
       3
     }",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          IDENT_EXPR
            IDENT 'println'
        NL_KW '\n'
        WHITESPACE '     '
        EXPR_ITEM
          BLOCK_EXPR
            OPEN_CURLY '{'
            NL_KW '\n'
            WHITESPACE '       '
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '3'
            NL_KW '\n'
            WHITESPACE '     '
            CLOSE_CURLY '}'
    "#],
  );

  // NOTE: This should parse as two expressions lol
  check(
    "println

     {
       3
     }",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          IDENT_EXPR
            IDENT 'println'
        NL_KW '\n'
        NL_KW '\n'
        WHITESPACE '     '
        EXPR_ITEM
          BLOCK_EXPR
            OPEN_CURLY '{'
            NL_KW '\n'
            WHITESPACE '       '
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '3'
            NL_KW '\n'
            WHITESPACE '     '
            CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn new_expr() {
  check(
    "new Iterator",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          NEW_EXPR
            NEW_KW 'new'
            WHITESPACE ' '
            IDENT 'Iterator'
    "#],
  );

  check(
    "new Iterator()",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          NEW_EXPR
            NEW_KW 'new'
            WHITESPACE ' '
            IDENT 'Iterator'
            PAREN_ARGUMENTS
              OPEN_PAREN '('
              CLOSE_PAREN ')'
    "#],
  );

  check(
    "new Iterator(2, 3)",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          NEW_EXPR
            NEW_KW 'new'
            WHITESPACE ' '
            IDENT 'Iterator'
            PAREN_ARGUMENTS
              OPEN_PAREN '('
              LIT_EXPR
                INT_LIT_KW '2'
              COMMA ','
              WHITESPACE ' '
              LIT_EXPR
                INT_LIT_KW '3'
              CLOSE_PAREN ')'
    "#],
  );

  check(
    "new Iterator { def next = None }",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          NEW_EXPR
            NEW_KW 'new'
            WHITESPACE ' '
            IDENT 'Iterator'
            WHITESPACE ' '
            BLOCK_EXPR
              OPEN_CURLY '{'
              WHITESPACE ' '
              FUN_DEF
                DEF_KW 'def'
                WHITESPACE ' '
                FUN_SIG
                  IDENT 'next'
                WHITESPACE ' '
                EQ '='
                WHITESPACE ' '
                IDENT_EXPR
                  IDENT 'None'
              WHITESPACE ' '
              CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn match_exprs() {
  check(
    "2 match {
      case 1 => 3
      case 2 => 4
      case _ => 5
    }",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          MATCH_EXPR
            LIT_EXPR
              INT_LIT_KW '2'
            WHITESPACE ' '
            MATCH_KW 'match'
            WHITESPACE ' '
            OPEN_CURLY '{'
            NL_KW '\n'
            WHITESPACE '      '
            CASE_ITEM
              CASE_KW 'case'
              WHITESPACE ' '
              LIT_PATTERN
                INT_LIT_KW '1'
              WHITESPACE ' '
              FAT_ARROW '=>'
              WHITESPACE ' '
              BLOCK
                EXPR_ITEM
                  LIT_EXPR
                    INT_LIT_KW '3'
                NL_KW '\n'
            WHITESPACE '      '
            CASE_ITEM
              CASE_KW 'case'
              WHITESPACE ' '
              LIT_PATTERN
                INT_LIT_KW '2'
              WHITESPACE ' '
              FAT_ARROW '=>'
              WHITESPACE ' '
              BLOCK
                EXPR_ITEM
                  LIT_EXPR
                    INT_LIT_KW '4'
                NL_KW '\n'
            WHITESPACE '      '
            CASE_ITEM
              CASE_KW 'case'
              WHITESPACE ' '
              PATH_PATTERN
                PATH
                  IDENT '_'
              WHITESPACE ' '
              FAT_ARROW '=>'
              WHITESPACE ' '
              BLOCK
                EXPR_ITEM
                  LIT_EXPR
                    INT_LIT_KW '5'
                NL_KW '\n'
            WHITESPACE '    '
            CLOSE_CURLY '}'
    "#],
  );

  check(
    "2 match { case 1 => 3 }",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          MATCH_EXPR
            LIT_EXPR
              INT_LIT_KW '2'
            WHITESPACE ' '
            MATCH_KW 'match'
            WHITESPACE ' '
            OPEN_CURLY '{'
            WHITESPACE ' '
            CASE_ITEM
              CASE_KW 'case'
              WHITESPACE ' '
              LIT_PATTERN
                INT_LIT_KW '1'
              WHITESPACE ' '
              FAT_ARROW '=>'
              WHITESPACE ' '
              BLOCK
                EXPR_ITEM
                  LIT_EXPR
                    INT_LIT_KW '3'
            WHITESPACE ' '
            CLOSE_CURLY '}'
    "#],
  );

  check(
    "2 match { case 0 => 1\n case 2 => 3 }",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          MATCH_EXPR
            LIT_EXPR
              INT_LIT_KW '2'
            WHITESPACE ' '
            MATCH_KW 'match'
            WHITESPACE ' '
            OPEN_CURLY '{'
            WHITESPACE ' '
            CASE_ITEM
              CASE_KW 'case'
              WHITESPACE ' '
              LIT_PATTERN
                INT_LIT_KW '0'
              WHITESPACE ' '
              FAT_ARROW '=>'
              WHITESPACE ' '
              BLOCK
                EXPR_ITEM
                  LIT_EXPR
                    INT_LIT_KW '1'
                NL_KW '\n'
            WHITESPACE ' '
            CASE_ITEM
              CASE_KW 'case'
              WHITESPACE ' '
              LIT_PATTERN
                INT_LIT_KW '2'
              WHITESPACE ' '
              FAT_ARROW '=>'
              WHITESPACE ' '
              BLOCK
                EXPR_ITEM
                  LIT_EXPR
                    INT_LIT_KW '3'
            WHITESPACE ' '
            CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn tuple_exprs() {
  check(
    "()",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          TUPLE_EXPR
            OPEN_PAREN '('
            CLOSE_PAREN ')'
    "#],
  );

  check(
    "(2, 3)",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          TUPLE_EXPR
            OPEN_PAREN '('
            LIT_EXPR
              INT_LIT_KW '2'
            COMMA ','
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
            CLOSE_PAREN ')'
    "#],
  );
}

#[test]
fn prefix_ops() {
  check(
    "!true > 3",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          INFIX_EXPR
            PREFIX_EXPR
              IDENT '!'
              LIT_EXPR
                TRUE_KW 'true'
            WHITESPACE ' '
            IDENT '>'
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
    "#],
  );

  check(
    "!foo.bar.baz",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          PREFIX_EXPR
            IDENT '!'
            FIELD_EXPR
              FIELD_EXPR
                IDENT_EXPR
                  IDENT 'foo'
                DOT '.'
                IDENT 'bar'
              DOT '.'
              IDENT 'baz'
    "#],
  );
}

#[test]
fn if_expr() {
  check(
    "if (2 == 3) { 4 } else { 5 }",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          IF_EXPR
            IF_KW 'if'
            WHITESPACE ' '
            OPEN_PAREN '('
            INFIX_EXPR
              LIT_EXPR
                INT_LIT_KW '2'
              WHITESPACE ' '
              IDENT '=='
              WHITESPACE ' '
              LIT_EXPR
                INT_LIT_KW '3'
            CLOSE_PAREN ')'
            WHITESPACE ' '
            BLOCK_EXPR
              OPEN_CURLY '{'
              WHITESPACE ' '
              EXPR_ITEM
                LIT_EXPR
                  INT_LIT_KW '4'
              WHITESPACE ' '
              CLOSE_CURLY '}'
            WHITESPACE ' '
            ELSE_KW 'else'
            WHITESPACE ' '
            BLOCK_EXPR
              OPEN_CURLY '{'
              WHITESPACE ' '
              EXPR_ITEM
                LIT_EXPR
                  INT_LIT_KW '5'
              WHITESPACE ' '
              CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn bare_assignment() {
  check(
    "x = 3",
    expect![@r#"
      SOURCE_FILE
        EXPR_ITEM
          ASSIGN_EXPR
            IDENT_EXPR
              IDENT 'x'
            WHITESPACE ' '
            EQ '='
            WHITESPACE ' '
            LIT_EXPR
              INT_LIT_KW '3'
    "#],
  );
}
