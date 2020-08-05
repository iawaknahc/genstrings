type routine_call = { key : string; comment : string; pos : Lexing.position }

type new_value = Key | Comment | String of string

exception ManyError of exn list

exception InconsistentComment of routine_call * routine_call

exception MissingDevLang

exception MoreThanOneDevLang of string list

val genstrings :
  routine_name:string -> devlang:string -> new_value:new_value -> string -> unit
