type comments = string list

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

exception UnterminatedComment

exception UnterminatedStringLiteral

exception InvalidEscapeSequence

val lex : Lexing.lexbuf -> token
