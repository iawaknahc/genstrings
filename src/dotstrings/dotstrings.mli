type t = entry list

and entry = { comment : string; key : string; value : string }

exception ParseError of string * Lexing.position * Lexing.position

val parse_string : ?filename:string -> string -> t

val pp : Format.formatter -> t -> unit
