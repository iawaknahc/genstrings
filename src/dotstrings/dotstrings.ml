module Ast = Dotstrings_ast
module I = Dotstrings_parse.MenhirInterpreter

exception ParseError of string * Lexing.position * Lexing.position

let initial_position filename =
  {Lexing.pos_fname= filename; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}

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

let parse_string ?(filename = "<stdin>") s =
  let lexbuf = Lexing.from_string s in
  let lex = Dotstrings_lex.new_raw () in
  let checkpoint =
    Dotstrings_parse.Incremental.plist @@ initial_position filename
  in
  loop lex lexbuf checkpoint
