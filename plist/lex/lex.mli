exception UnterminatedBlockComment

exception UnterminatedStringLiteral

exception UnterminatedBytes

exception InvalidEscapeSequence of string

exception InvalidCharacter of char

val new_raw :
  unit -> Lexing.lexbuf -> Token.token * Lexing.position * Lexing.position
