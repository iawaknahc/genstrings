include Dotstrings_ast
module I = Dotstrings_parse.MenhirInterpreter

exception ParseError of string * Lexing.position * Lexing.position

let initial_position =
  {Lexing.pos_fname= ""; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}

let string_of_exn exn =
  let open Dotstrings_lex in
  match exn with
  | UnterminatedBlockComment -> "unterminated block comment"
  | UnterminatedStringLiteral -> "unterminated string literal"
  | UnterminatedBytes -> "unterminated bytes"
  | InvalidEscapeSequence s -> "invalid escape sequence `" ^ s ^ "'"
  | InvalidCharacter ch -> "invalid character `" ^ String.make 1 ch ^ "'"
  | _ -> "unknown error"

let rec loop lex lexbuf checkpoint =
  match checkpoint with
  | I.InputNeeded _ ->
      let result =
        try lex lexbuf
        with e ->
          let start = lexbuf.Lexing.lex_start_p in
          let end_ = lexbuf.Lexing.lex_curr_p in
          raise @@ ParseError (string_of_exn e, start, end_)
      in
      let checkpoint = I.offer checkpoint result in
      loop lex lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ -> loop lex lexbuf (I.resume checkpoint)
  | I.HandlingError env ->
      let start, end_ = I.positions env in
      let state = I.current_state_number env in
      let message =
        try Error.message state with Not_found -> "unknown error"
      in
      let message = String.trim message in
      raise @@ ParseError (message, start, end_)
  | I.Accepted v -> v
  | I.Rejected -> assert false

let parse_string s =
  let lexbuf = Lexing.from_string s in
  let lex = Dotstrings_lex.new_raw () in
  let checkpoint = Dotstrings_parse.Incremental.plist initial_position in
  loop lex lexbuf checkpoint

(* TODO quote *)
let quote s = s

let write_channel ch t =
  List.iter
    (fun entry ->
      output_string ch "/*" ;
      output_string ch entry.comment ;
      output_string ch "*/\n" ;
      let key = quote entry.key in
      output_string ch key ;
      output_string ch " = " ;
      let value = quote entry.value in
      output_string ch value ; output_string ch ";\n\n" )
    t
