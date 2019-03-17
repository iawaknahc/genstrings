exception ParseError of string * Lexing.position * Lexing.position

val parse_string : string -> Ast.t
