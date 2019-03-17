type raw_token =
  | EOF
  | BareString of string
  | QuotedString of string
  | Bytes of bytes
  | BlockComment of string
  | LineComment of string
  | Semicolon
  | Equal
  | BraceLeft
  | BraceRight
  | ParenLeft
  | ParenRight
  | Comma

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

exception UnterminatedBlockComment

exception UnterminatedStringLiteral

exception UnterminatedBytes

exception InvalidEscapeSequence

exception InvalidCharacter of char

val new_raw :
  unit -> Lexing.lexbuf -> raw_token * Lexing.position * Lexing.position

val new_lex :
  unit -> Lexing.lexbuf -> token * Lexing.position * Lexing.position
