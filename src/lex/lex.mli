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

exception UnterminatedBytes

exception InvalidEscapeSequence

exception InvalidCharacter of char

val new_lex : unit -> Lexing.lexbuf -> t
