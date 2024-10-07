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
            INT_LIT_KW '1000L'
        NL_KW '\n'
    "#],
  );
}

#[test]
fn duration_multiply() {
  check(
    r#"
      factor == 0d || JDouble.isNaN(factor)
    "#,
    expect![@r#"
      SOURCE_FILE
        NL_KW '\n'
        WHITESPACE '      '
        EXPR_ITEM
          INFIX_EXPR
            INFIX_EXPR
              IDENT_EXPR
                IDENT 'factor'
              WHITESPACE ' '
              IDENT '=='
              WHITESPACE ' '
              LIT_EXPR
                FLOAT_LIT_KW '0d'
            WHITESPACE ' '
            IDENT '||'
            WHITESPACE ' '
            CALL_EXPR
              FIELD_EXPR
                IDENT_EXPR
                  IDENT 'JDouble'
                DOT '.'
                IDENT 'isNaN'
              PAREN_ARGUMENTS
                OPEN_PAREN '('
                IDENT_EXPR
                  IDENT 'factor'
                CLOSE_PAREN ')'
        NL_KW '\n'
    "#],
  );
}

#[test]
fn string_ctx_invalid_escape() {
  check(
    r#"
      s"""invalid escape ${
        require(index >= 0 && index < str.length)
        val ok = s"""[\\b, \\t, \\n, \\f, \\r, \\\\, \\", \\', \\uxxxx]"""
        if (index == str.length - 1) "at terminal" else s"'\\${str(index + 1)}' not one of $ok at"
      } index $index in "$str". Use \\\\ for literal \\."""
    "#,
    expect![@r#"
      SOURCE_FILE
        NL_KW '\n'
        WHITESPACE '      '
        EXPR_ITEM
          INTERPOLATED_STRING
            IDENT 's'
            DOUBLE_QUOTE '"'
            DOUBLE_QUOTE '"'
            DOUBLE_QUOTE '"'
            IDENT 'invalid'
            WHITESPACE ' '
            IDENT 'e'
            IDENT 's'
            IDENT 'c'
            IDENT 'a'
            IDENT 'p'
            IDENT 'e'
            WHITESPACE ' '
            BLOCK_EXPR
              IDENT '$'
              OPEN_CURLY '{'
              NL_KW '\n'
              WHITESPACE '        '
              EXPR_ITEM
                CALL_EXPR
                  IDENT_EXPR
                    IDENT 'require'
                  PAREN_ARGUMENTS
                    OPEN_PAREN '('
                    INFIX_EXPR
                      INFIX_EXPR
                        IDENT_EXPR
                          IDENT 'index'
                        WHITESPACE ' '
                        IDENT '>='
                        WHITESPACE ' '
                        LIT_EXPR
                          INT_LIT_KW '0'
                      WHITESPACE ' '
                      IDENT '&&'
                      WHITESPACE ' '
                      INFIX_EXPR
                        IDENT_EXPR
                          IDENT 'index'
                        WHITESPACE ' '
                        IDENT '<'
                        WHITESPACE ' '
                        FIELD_EXPR
                          IDENT_EXPR
                            IDENT 'str'
                          DOT '.'
                          IDENT 'length'
                    CLOSE_PAREN ')'
              NL_KW '\n'
              WHITESPACE '        '
              VAL_DEF
                VAL_KW 'val'
                WHITESPACE ' '
                IDENT 'ok'
                WHITESPACE ' '
                EQ '='
                WHITESPACE ' '
                INTERPOLATED_STRING
                  IDENT 's'
                  DOUBLE_QUOTE '"'
                  DOUBLE_QUOTE '"'
                  DOUBLE_QUOTE '"'
                  OPEN_BRACKET '['
                  IDENT '\'
                  IDENT '\'
                  IDENT 'b'
                  COMMA ','
                  WHITESPACE ' '
                  IDENT '\'
                  IDENT '\'
                  IDENT 't'
                  COMMA ','
                  WHITESPACE ' '
                  IDENT '\'
                  IDENT '\'
                  IDENT 'n'
                  COMMA ','
                  WHITESPACE ' '
                  IDENT '\'
                  IDENT '\'
                  IDENT 'f'
                  COMMA ','
                  WHITESPACE ' '
                  IDENT '\'
                  IDENT '\'
                  IDENT 'r'
                  COMMA ','
                  WHITESPACE ' '
                  IDENT '\'
                  IDENT '\'
                  IDENT '\'
                  IDENT '\'
                  COMMA ','
                  WHITESPACE ' '
                  IDENT '\'
                  IDENT '\'
                  DOUBLE_QUOTE '"'
                  COMMA ','
                  WHITESPACE ' '
                  IDENT '\'
                  IDENT '\'
                  SINGLE_QUOTE '''
                  COMMA ','
                  WHITESPACE ' '
                  IDENT '\'
                  IDENT '\'
                  IDENT 'u'
                  IDENT 'x'
                  IDENT 'x'
                  IDENT 'x'
                  IDENT 'x'
                  CLOSE_BRACKET ']'
                  DOUBLE_QUOTE '"'
                  DOUBLE_QUOTE '"'
                  DOUBLE_QUOTE '"'
              NL_KW '\n'
              WHITESPACE '        '
              EXPR_ITEM
                IF_EXPR
                  IF_KW 'if'
                  WHITESPACE ' '
                  OPEN_PAREN '('
                  INFIX_EXPR
                    IDENT_EXPR
                      IDENT 'index'
                    WHITESPACE ' '
                    IDENT '=='
                    WHITESPACE ' '
                    INFIX_EXPR
                      FIELD_EXPR
                        IDENT_EXPR
                          IDENT 'str'
                        DOT '.'
                        IDENT 'length'
                      WHITESPACE ' '
                      IDENT '-'
                      WHITESPACE ' '
                      LIT_EXPR
                        INT_LIT_KW '1'
                  CLOSE_PAREN ')'
                  WHITESPACE ' '
                  DOUBLE_QUOTED_STRING
                    DOUBLE_QUOTE '"'
                    IDENT 'a'
                    IDENT 't'
                    WHITESPACE ' '
                    IDENT 't'
                    IDENT 'e'
                    IDENT 'r'
                    IDENT 'm'
                    IDENT 'i'
                    IDENT 'n'
                    IDENT 'a'
                    IDENT 'l'
                    DOUBLE_QUOTE '"'
                  WHITESPACE ' '
                  ELSE_KW 'else'
                  WHITESPACE ' '
                  INTERPOLATED_STRING
                    IDENT 's'
                    DOUBLE_QUOTE '"'
                    SINGLE_QUOTE '''
                    IDENT '\'
                    IDENT '\'
                    BLOCK_EXPR
                      IDENT '$'
                      OPEN_CURLY '{'
                      EXPR_ITEM
                        CALL_EXPR
                          IDENT_EXPR
                            IDENT 'str'
                          PAREN_ARGUMENTS
                            OPEN_PAREN '('
                            INFIX_EXPR
                              IDENT_EXPR
                                IDENT 'index'
                              WHITESPACE ' '
                              IDENT '+'
                              WHITESPACE ' '
                              LIT_EXPR
                                INT_LIT_KW '1'
                            CLOSE_PAREN ')'
                      CLOSE_CURLY '}'
                    SINGLE_QUOTE '''
                    WHITESPACE ' '
                    IDENT 'n'
                    IDENT 'o'
                    IDENT 't'
                    WHITESPACE ' '
                    IDENT 'o'
                    IDENT 'n'
                    IDENT 'e'
                    WHITESPACE ' '
                    IDENT 'o'
                    IDENT 'f'
                    WHITESPACE ' '
                    IDENT '$'
                    IDENT 'o'
                    IDENT 'k'
                    WHITESPACE ' '
                    IDENT 'a'
                    IDENT 't'
                    DOUBLE_QUOTE '"'
              NL_KW '\n'
              WHITESPACE '      '
              CLOSE_CURLY '}'
            WHITESPACE ' '
            IDENT 'i'
            IDENT 'n'
            IDENT 'd'
            IDENT 'e'
            IDENT 'x'
            WHITESPACE ' '
            IDENT '$'
            IDENT 'i'
            IDENT 'n'
            IDENT 'd'
            IDENT 'e'
            IDENT 'x'
            WHITESPACE ' '
            IDENT 'i'
            IDENT 'n'
            WHITESPACE ' '
            DOUBLE_QUOTE '"'
            IDENT '$'
            IDENT 's'
            IDENT 't'
            IDENT 'r'
            DOUBLE_QUOTE '"'
            DOT '.'
            WHITESPACE ' '
            IDENT 'U'
            IDENT 's'
            IDENT 'e'
            WHITESPACE ' '
            IDENT '\'
            IDENT '\'
            IDENT '\'
            IDENT '\'
            WHITESPACE ' '
            IDENT 'f'
            IDENT 'o'
            IDENT 'r'
            WHITESPACE ' '
            IDENT 'l'
            IDENT 'i'
            IDENT 't'
            IDENT 'e'
            IDENT 'r'
            IDENT 'a'
            IDENT 'l'
            WHITESPACE ' '
            IDENT '\'
            IDENT '\'
            DOT '.'
            DOUBLE_QUOTE '"'
            DOUBLE_QUOTE '"'
            DOUBLE_QUOTE '"'
        NL_KW '\n'
    "#],
  );
}
