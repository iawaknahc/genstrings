type t =
  | EOF
  | Whitespace of string
  | BareString of string
  | QuotedString of string
  | Comment of string
  | Semicolon
  | Equal
  | BraceLeft
  | BraceRight
  | ParenLeft
  | ParenRight
  | Comma
  | LessThan
  | GreaterThan

exception UnterminatedComment

exception UnterminatedStringLiteral

exception InvalidEscapeSequence

val lex : Lexing.lexbuf -> t
