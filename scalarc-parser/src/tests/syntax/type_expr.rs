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
fn type_params() {
  check(
    "val foo: Foo[] = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          IDENT 'foo'
          COLON ':'
          WHITESPACE ' '
          GENERIC_TYPE
            SIMPLE_TYPE
              IDENT 'Foo'
            TYPE_PARAMS
              OPEN_BRACKET '['
              SIMPLE_TYPE
                error: expected IDENT
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
          IDENT 'foo'
          COLON ':'
          WHITESPACE ' '
          GENERIC_TYPE
            SIMPLE_TYPE
              IDENT 'Foo'
            TYPE_PARAMS
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
          IDENT 'foo'
          COLON ':'
          WHITESPACE ' '
          GENERIC_TYPE
            SIMPLE_TYPE
              IDENT 'Foo'
            TYPE_PARAMS
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
