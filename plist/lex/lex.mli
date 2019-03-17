exception UnterminatedBlockComment

exception UnterminatedStringLiteral

exception UnterminatedBytes

exception InvalidEscapeSequence

exception InvalidCharacter of char

val new_raw :
  unit -> Lexing.lexbuf -> Token.token * Lexing.position * Lexing.position
