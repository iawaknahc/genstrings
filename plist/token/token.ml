type comment = BlockComment of string | LineComment of string

type comments = comment list

type token =
  | EOF of comments
  | BareString of string * comments
  | QuotedString of string * comments
  | Bytes of bytes * comments
  | Semicolon of comments
  | Equal of comments
  | BraceLeft of comments
  | BraceRight of comments
  | ParenLeft of comments
  | ParenRight of comments
  | Comma of comments
