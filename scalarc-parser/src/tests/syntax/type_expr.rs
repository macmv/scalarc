use crate::tests::check;

#[test]
fn type_paths() {
  check(
    "val foo: Foo.Bar = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          TYPE_PATTERN
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            PATH_TYPE
              SIMPLE_TYPE
                IDENT 'Foo'
              DOT '.'
              IDENT 'Bar'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "val foo: Foo.Bar.Baz = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          TYPE_PATTERN
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            PATH_TYPE
              PATH_TYPE
                SIMPLE_TYPE
                  IDENT 'Foo'
                DOT '.'
                IDENT 'Bar'
              DOT '.'
              IDENT 'Baz'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );
}

#[test]
fn type_args() {
  check(
    "val foo: Foo[] = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          TYPE_PATTERN
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            GENERIC_TYPE
              SIMPLE_TYPE
                IDENT 'Foo'
              TYPE_ARGS
                OPEN_BRACKET '['
                error: expected type
                CLOSE_BRACKET ']'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "val foo: Foo[Int] = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          TYPE_PATTERN
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            GENERIC_TYPE
              SIMPLE_TYPE
                IDENT 'Foo'
              TYPE_ARGS
                OPEN_BRACKET '['
                SIMPLE_TYPE
                  IDENT 'Int'
                CLOSE_BRACKET ']'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "val foo: Foo[Int, String] = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          TYPE_PATTERN
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            GENERIC_TYPE
              SIMPLE_TYPE
                IDENT 'Foo'
              TYPE_ARGS
                OPEN_BRACKET '['
                SIMPLE_TYPE
                  IDENT 'Int'
                COMMA ','
                WHITESPACE ' '
                SIMPLE_TYPE
                  IDENT 'String'
                CLOSE_BRACKET ']'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );
}

#[test]
fn type_params() {
  check(
    "def foo[A <: Int](a: A) = 3",
    expect![@r#"
      SOURCE_FILE
        FUN_DEF
          DEF_KW 'def'
          WHITESPACE ' '
          FUN_SIG
            IDENT 'foo'
            TYPE_PARAMS
              OPEN_BRACKET '['
              LOWER_BOUND_PARAM
                SIMPLE_TYPE
                  IDENT 'A'
                WHITESPACE ' '
                LESS_COLON '<:'
                WHITESPACE ' '
                SIMPLE_TYPE
                  IDENT 'Int'
              CLOSE_BRACKET ']'
            FUN_PARAMS
              OPEN_PAREN '('
              FUN_PARAM
                IDENT 'a'
                COLON ':'
                WHITESPACE ' '
                SIMPLE_TYPE
                  IDENT 'A'
              CLOSE_PAREN ')'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );
}

#[test]
fn tuple() {
  check(
    "val foo: (Int, String) = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          TYPE_PATTERN
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            TUPLE_TYPE
              OPEN_PAREN '('
              SIMPLE_TYPE
                IDENT 'Int'
              COMMA ','
              WHITESPACE ' '
              SIMPLE_TYPE
                IDENT 'String'
              CLOSE_PAREN ')'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );
}

#[test]
fn lambda() {
  check(
    "val foo: (Int, String) => Int = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          TYPE_PATTERN
            IDENT 'foo'
            COLON ':'
            WHITESPACE ' '
            LAMBDA_TYPE
              TUPLE_TYPE
                OPEN_PAREN '('
                SIMPLE_TYPE
                  IDENT 'Int'
                COMMA ','
                WHITESPACE ' '
                SIMPLE_TYPE
                  IDENT 'String'
                CLOSE_PAREN ')'
              WHITESPACE ' '
              FAT_ARROW '=>'
              WHITESPACE ' '
              SIMPLE_TYPE
                IDENT 'Int'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );
}
