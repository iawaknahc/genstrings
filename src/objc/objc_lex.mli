exception UnterminatedBlockComment
exception UnterminatedStringLiteral
exception IllegalEscapeSequence

val token :
  Lexing.lexbuf -> Objc_parse.token * Lexing.position * Lexing.position
