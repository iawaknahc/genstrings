type t =
  | EOF
  | BareString of string
  | QuotedString of string
  | Bytes of bytes
  | Comment of string
  | Semicolon
  | Equal
  | BraceLeft
  | BraceRight
  | ParenLeft
  | ParenRight
  | Comma

exception UnterminatedComment

exception UnterminatedStringLiteral

exception InvalidEscapeSequence

val lex : Lexing.lexbuf -> t
