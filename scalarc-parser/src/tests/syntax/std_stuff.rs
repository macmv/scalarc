use crate::tests::check;

// This file contains some examples from `std` that are not covered in other
// tests.

#[test]
fn duration_words() {
  check(
    r#"
      private[this] def expandLabels(labels: String): List[String] = {
        val hd :: rest = words(labels): @unchecked
        hd :: rest.flatMap(s => List(s, s + "s"))
      }
    "#,
    expect![@r#"
      SOURCE_FILE
        NL_KW '\n'
        WHITESPACE '      '
        FUN_DEF
          MODIFIER
            PRIVATE_KW 'private'
            ACCESS_QUALIFIER
              OPEN_BRACKET '['
              IDENT 'this'
              CLOSE_BRACKET ']'
          WHITESPACE ' '
          DEF_KW 'def'
          WHITESPACE ' '
          FUN_SIG
            IDENT 'expandLabels'
            FUN_PARAMS
              OPEN_PAREN '('
              FUN_PARAM
                IDENT 'labels'
                COLON ':'
                WHITESPACE ' '
                SIMPLE_TYPE
                  IDENT 'String'
              CLOSE_PAREN ')'
            COLON ':'
            WHITESPACE ' '
            GENERIC_TYPE
              SIMPLE_TYPE
                IDENT 'List'
              TYPE_ARGS
                OPEN_BRACKET '['
                SIMPLE_TYPE
                  IDENT 'String'
                CLOSE_BRACKET ']'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          BLOCK_EXPR
            OPEN_CURLY '{'
            NL_KW '\n'
            WHITESPACE '        '
            VAL_DEF
              VAL_KW 'val'
              WHITESPACE ' '
              INFIX_PATTERN
                PATH_PATTERN
                  PATH
                    IDENT 'hd'
                WHITESPACE ' '
                IDENT '::'
                WHITESPACE ' '
                PATH_PATTERN
                  PATH
                    IDENT 'rest'
              WHITESPACE ' '
              EQ '='
              WHITESPACE ' '
              ASCRIPT_EXPR
                CALL_EXPR
                  IDENT_EXPR
                    IDENT 'words'
                  PAREN_ARGUMENTS
                    OPEN_PAREN '('
                    IDENT_EXPR
                      IDENT 'labels'
                    CLOSE_PAREN ')'
                COLON ':'
                WHITESPACE ' '
                ANNOTATION_ASCRIPTION
                  AT '@'
                  SIMPLE_TYPE
                    IDENT 'unchecked'
            NL_KW '\n'
            WHITESPACE '        '
            EXPR_ITEM
              INFIX_EXPR
                IDENT_EXPR
                  IDENT 'hd'
                WHITESPACE ' '
                IDENT '::'
                WHITESPACE ' '
                CALL_EXPR
                  FIELD_EXPR
                    IDENT_EXPR
                      IDENT 'rest'
                    DOT '.'
                    IDENT 'flatMap'
                  PAREN_ARGUMENTS
                    OPEN_PAREN '('
                    LAMBDA_EXPR
                      IDENT_EXPR
                        IDENT 's'
                      WHITESPACE ' '
                      FAT_ARROW '=>'
                      WHITESPACE ' '
                      BLOCK
                        EXPR_ITEM
                          CALL_EXPR
                            IDENT_EXPR
                              IDENT 'List'
                            PAREN_ARGUMENTS
                              OPEN_PAREN '('
                              IDENT_EXPR
                                IDENT 's'
                              COMMA ','
                              WHITESPACE ' '
                              INFIX_EXPR
                                IDENT_EXPR
                                  IDENT 's'
                                WHITESPACE ' '
                                IDENT '+'
                                WHITESPACE ' '
                                DOUBLE_QUOTED_STRING
                                  DOUBLE_QUOTE '"'
                                  IDENT 's'
                                  DOUBLE_QUOTE '"'
                              CLOSE_PAREN ')'
                    CLOSE_PAREN ')'
            NL_KW '\n'
            WHITESPACE '      '
            CLOSE_CURLY '}'
        NL_KW '\n'
    "#],
  );
}

#[test]
fn duration_micros() {
  check(
    r#"
      private[this] final val ns_per_µs  = 1000L
    "#,
    expect![@r#"
      SOURCE_FILE
        NL_KW '\n'
        WHITESPACE '      '
        VAL_DEF
          MODIFIER
            PRIVATE_KW 'private'
            ACCESS_QUALIFIER
              OPEN_BRACKET '['
              IDENT 'this'
              CLOSE_BRACKET ']'
          WHITESPACE ' '
          MODIFIER
            FINAL_KW 'final'
          WHITESPACE ' '
          VAL_KW 'val'
          WHITESPACE ' '
          IDENT 'ns_per_µs'
          WHITESPACE '  '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '1000'
            IDENT 'L'
        NL_KW '\n'
    "#],
  );
}
