use crate::tests::check;

#[test]
fn imports() {
  check(
    "import foo.bar.baz",
    expect![@r#"
      SOURCE_FILE
        IMPORT
          IMPORT_KW 'import'
          WHITESPACE ' '
          PATH
            IDENT 'foo'
            DOT '.'
            IDENT 'bar'
            DOT '.'
            IDENT 'baz'
    "#],
  );

  check(
    "import foo.{ bar, baz }",
    expect![@r#"
      SOURCE_FILE
        IMPORT
          IMPORT_KW 'import'
          WHITESPACE ' '
          IMPORT_SELECTORS
            PATH
              IDENT 'foo'
              DOT '.'
            OPEN_CURLY '{'
            WHITESPACE ' '
            IMPORT_SELECTOR_ID
              IDENT 'bar'
            COMMA ','
            WHITESPACE ' '
            IMPORT_SELECTOR_ID
              IDENT 'baz'
            WHITESPACE ' '
            CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn fun_dec() {
  check(
    "def foo = 3",
    expect![@r#"
      SOURCE_FILE
        FUN_DEF
          DEF_KW 'def'
          WHITESPACE ' '
          FUN_SIG
            IDENT 'foo'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "def foo(a:) = 3",
    expect![@r#"
      SOURCE_FILE
        FUN_DEF
          DEF_KW 'def'
          WHITESPACE ' '
          FUN_SIG
            IDENT 'foo'
            FUN_PARAMS
              OPEN_PAREN '('
              FUN_PARAM
                IDENT 'a'
                COLON ':'
                SIMPLE_TYPE
                  error: expected IDENT
              CLOSE_PAREN ')'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "def foo(a: Int) = 3",
    expect![@r#"
      SOURCE_FILE
        FUN_DEF
          DEF_KW 'def'
          WHITESPACE ' '
          FUN_SIG
            IDENT 'foo'
            FUN_PARAMS
              OPEN_PAREN '('
              FUN_PARAM
                IDENT 'a'
                COLON ':'
                WHITESPACE ' '
                SIMPLE_TYPE
                  IDENT 'Int'
              CLOSE_PAREN ')'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "def foo(a: Int, b: Boolean = true) = 3",
    expect![@r#"
      SOURCE_FILE
        FUN_DEF
          DEF_KW 'def'
          WHITESPACE ' '
          FUN_SIG
            IDENT 'foo'
            FUN_PARAMS
              OPEN_PAREN '('
              FUN_PARAM
                IDENT 'a'
                COLON ':'
                WHITESPACE ' '
                SIMPLE_TYPE
                  IDENT 'Int'
              COMMA ','
              WHITESPACE ' '
              FUN_PARAM
                IDENT 'b'
                COLON ':'
                WHITESPACE ' '
                SIMPLE_TYPE
                  IDENT 'Boolean'
                WHITESPACE ' '
                EQ '='
                WHITESPACE ' '
                LIT_EXPR
                  TRUE_KW 'true'
              CLOSE_PAREN ')'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "def foo[a <: Int] = 3",
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
                  IDENT 'a'
                WHITESPACE ' '
                LESS_COLON '<:'
                WHITESPACE ' '
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
}

#[test]
fn newline_handling() {
  check(
    "class Foo() { class Bar() {} class Baz() {} }",
    expect![@r#"
      SOURCE_FILE
        CLASS_DEF
          CLASS_KW 'class'
          WHITESPACE ' '
          IDENT 'Foo'
          FUN_PARAMS
            OPEN_PAREN '('
            CLOSE_PAREN ')'
          WHITESPACE ' '
          ITEM_BODY
            OPEN_CURLY '{'
            WHITESPACE ' '
            CLASS_DEF
              CLASS_KW 'class'
              WHITESPACE ' '
              IDENT 'Bar'
              FUN_PARAMS
                OPEN_PAREN '('
                CLOSE_PAREN ')'
              WHITESPACE ' '
              ITEM_BODY
                OPEN_CURLY '{'
                CLOSE_CURLY '}'
            error: expected newline
            WHITESPACE ' '
            CLASS_DEF
              CLASS_KW 'class'
              WHITESPACE ' '
              IDENT 'Baz'
              FUN_PARAMS
                OPEN_PAREN '('
                CLOSE_PAREN ')'
              WHITESPACE ' '
              ITEM_BODY
                OPEN_CURLY '{'
                CLOSE_CURLY '}'
            WHITESPACE ' '
            CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn val_def() {
  check(
    "val foo = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          IDENT 'foo'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "val foo: Int = 3",
    expect![@r#"
      SOURCE_FILE
        VAL_DEF
          VAL_KW 'val'
          WHITESPACE ' '
          IDENT 'foo'
          COLON ':'
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

#[test]
fn var_def() {
  check(
    "var foo = 3",
    expect![@r#"
      SOURCE_FILE
        VAR_DEF
          VAR_KW 'var'
          WHITESPACE ' '
          IDENT 'foo'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          LIT_EXPR
            INT_LIT_KW '3'
    "#],
  );

  check(
    "var foo: Int = 3",
    expect![@r#"
      SOURCE_FILE
        VAR_DEF
          VAR_KW 'var'
          WHITESPACE ' '
          IDENT 'foo'
          COLON ':'
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

  // Vars allow `_` for null. We just parse `_` as an identifier, but this is
  // special cased in the real scala compiler.
  check(
    "var foo: Int = _",
    expect![@r#"
      SOURCE_FILE
        VAR_DEF
          VAR_KW 'var'
          WHITESPACE ' '
          IDENT 'foo'
          COLON ':'
          WHITESPACE ' '
          SIMPLE_TYPE
            IDENT 'Int'
          WHITESPACE ' '
          EQ '='
          WHITESPACE ' '
          IDENT_EXPR
            IDENT '_'
    "#],
  );
}

#[test]
fn class_def() {
  check(
    "class Foo() {}",
    expect![@r#"
      SOURCE_FILE
        CLASS_DEF
          CLASS_KW 'class'
          WHITESPACE ' '
          IDENT 'Foo'
          FUN_PARAMS
            OPEN_PAREN '('
            CLOSE_PAREN ')'
          WHITESPACE ' '
          ITEM_BODY
            OPEN_CURLY '{'
            CLOSE_CURLY '}'
    "#],
  );

  check(
    "class Foo() extends AnyVal {}",
    expect![@r#"
      SOURCE_FILE
        CLASS_DEF
          CLASS_KW 'class'
          WHITESPACE ' '
          IDENT 'Foo'
          FUN_PARAMS
            OPEN_PAREN '('
            CLOSE_PAREN ')'
          WHITESPACE ' '
          EXTENDS_KW 'extends'
          WHITESPACE ' '
          SIMPLE_TYPE
            IDENT 'AnyVal'
          WHITESPACE ' '
          ITEM_BODY
            OPEN_CURLY '{'
            CLOSE_CURLY '}'
    "#],
  );

  check(
    "class Foo() extends AnyVal with Foo {}",
    expect![@r#"
      SOURCE_FILE
        CLASS_DEF
          CLASS_KW 'class'
          WHITESPACE ' '
          IDENT 'Foo'
          FUN_PARAMS
            OPEN_PAREN '('
            CLOSE_PAREN ')'
          WHITESPACE ' '
          EXTENDS_KW 'extends'
          WHITESPACE ' '
          SIMPLE_TYPE
            IDENT 'AnyVal'
          WHITESPACE ' '
          WITH_KW 'with'
          WHITESPACE ' '
          SIMPLE_TYPE
            IDENT 'Foo'
          WHITESPACE ' '
          ITEM_BODY
            OPEN_CURLY '{'
            CLOSE_CURLY '}'
    "#],
  );

  check(
    "class Foo() extends AnyVal with Foo with Baz {}",
    expect![@r#"
      SOURCE_FILE
        CLASS_DEF
          CLASS_KW 'class'
          WHITESPACE ' '
          IDENT 'Foo'
          FUN_PARAMS
            OPEN_PAREN '('
            CLOSE_PAREN ')'
          WHITESPACE ' '
          EXTENDS_KW 'extends'
          WHITESPACE ' '
          SIMPLE_TYPE
            IDENT 'AnyVal'
          WHITESPACE ' '
          WITH_KW 'with'
          WHITESPACE ' '
          SIMPLE_TYPE
            IDENT 'Foo'
          WHITESPACE ' '
          WITH_KW 'with'
          WHITESPACE ' '
          SIMPLE_TYPE
            IDENT 'Baz'
          WHITESPACE ' '
          ITEM_BODY
            OPEN_CURLY '{'
            CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn object_works() {
  check(
    "object Foo {}",
    expect![@r#"
      SOURCE_FILE
        OBJECT_DEF
          OBJECT_KW 'object'
          WHITESPACE ' '
          IDENT 'Foo'
          WHITESPACE ' '
          ITEM_BODY
            OPEN_CURLY '{'
            CLOSE_CURLY '}'
    "#],
  );
}

#[test]
fn case_item() {
  check(
    "case _ if 2 == 3 => 4",
    expect![@r#"
      SOURCE_FILE
        CASE_ITEM
          CASE_KW 'case'
          WHITESPACE ' '
          IDENT_PATTERN
            IDENT '_'
          WHITESPACE ' '
          GUARD
            IF_KW 'if'
            WHITESPACE ' '
            INFIX_EXPR
              LIT_EXPR
                INT_LIT_KW '2'
              WHITESPACE ' '
              IDENT '=='
              WHITESPACE ' '
              LIT_EXPR
                INT_LIT_KW '3'
          WHITESPACE ' '
          FAT_ARROW '=>'
          WHITESPACE ' '
          BLOCK
            EXPR_ITEM
              LIT_EXPR
                INT_LIT_KW '4'
    "#],
  );
}
