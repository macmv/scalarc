//! Defines input for code generation process.

pub(crate) struct KindsSrc<'a> {
  pub(crate) punct:               &'a [(&'a str, &'a str)],
  pub(crate) keywords:            &'a [&'a str],
  pub(crate) contextual_keywords: &'a [&'a str],
  pub(crate) literals:            &'a [&'a str],
  pub(crate) tokens:              &'a [&'a str],
}

pub(crate) const KINDS_SRC: KindsSrc<'_> = KindsSrc {
  punct:               &[
    ("-", "MINUS"),
    ("+", "PLUS"),
    ("'{'", "OPEN_CURLY"),
    ("'}'", "CLOSE_CURLY"),
    ("'('", "OPEN_PAREN"),
    ("')'", "CLOSE_PAREN"),
    ("'['", "OPEN_BRACKET"),
    ("']'", "CLOSE_BRACKET"),
    ("<-", "THIN_LEFT_ARROW"),
    (">:", "GREATER_COLON"),
    ("<:", "LESS_COLON"),
    ("=", "EQ"),
    ("!", "BANG"),
    ("*", "STAR"),
    (".", "DOT"),
    ("=>", "FAT_ARROW"),
    ("@", "AT"),
    (":", "COLON"),
    ("#", "POUND"),
    (",", "COMMA"),
    ("~", "TILDE"),
    ("_", "UNDERSCORE"),
  ],
  keywords:            &[
    // control flow
    "if",
    "match",
    "case",
    "while",
    "try",
    "do",
    "for",
    "throw",
    "return",
    // exprs
    "this",
    "super",
    "type",
    "true",
    "false",
    "null",
    // statements
    "val",
    "var",
    "implicit",
    // items
    "class",
    "trait",
    "object",
    "package",
    "extends",
    "with",
    "def",
    "abstract",
    "final",
    "protected",
    "private",
    "sealed",
  ],
  contextual_keywords: &[
    "auto",
    "builtin",
    "default",
    "existential",
    "union",
    "raw",
    "macro_rules",
    "yeet",
    "offset_of",
    "asm",
    "format_args",
  ],
  literals:            &[
    "INT_NUMBER",
    "FLOAT_NUMBER",
    "CHAR",
    "BYTE",
    "STRING",
    "BYTE_STRING",
    "C_STRING",
  ],
  tokens:              &["ERROR", "IDENT", "WHITESPACE", "LIFETIME_IDENT", "COMMENT", "SHEBANG"],
};

#[derive(Default, Debug)]
pub(crate) struct AstSrc {
  pub(crate) tokens: Vec<String>,
  pub(crate) nodes:  Vec<AstNodeSrc>,
  pub(crate) enums:  Vec<AstEnumSrc>,
}

#[derive(Debug)]
pub(crate) struct AstNodeSrc {
  pub(crate) doc:    Vec<String>,
  pub(crate) name:   String,
  pub(crate) traits: Vec<String>,
  pub(crate) fields: Vec<Field>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Field {
  Token(String),
  Node { name: String, ty: String, cardinality: Cardinality },
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Cardinality {
  Optional,
  Many,
}

#[derive(Debug)]
pub(crate) struct AstEnumSrc {
  pub(crate) doc:      Vec<String>,
  pub(crate) name:     String,
  pub(crate) traits:   Vec<String>,
  pub(crate) variants: Vec<String>,
}
