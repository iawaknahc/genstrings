type t = item list

and item =
  | Ident of string * Lexing.position
  | Anything_else
  | Comma
  | Colon
  | L_paren
  | R_paren
  | At
  | String of string

exception ParseError of string * Lexing.position * Lexing.position

val parse_string : ?filename:string -> string -> t
