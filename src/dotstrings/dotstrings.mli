module Ast : sig
  type entry = {comment: string; key: string; value: string}
  type t = entry list
end

exception ParseError of string * Lexing.position * Lexing.position

val parse_string : ?filename:string -> string -> Ast.t
