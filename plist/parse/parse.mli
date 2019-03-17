exception ParseError of string * Lexing.position * Lexing.position

val parse_string : ?filename:string -> string -> Ast.t
