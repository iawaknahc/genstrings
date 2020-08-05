include Objc_ast
module I = Objc_parse.MenhirInterpreter

exception ParseError of string * Lexing.position * Lexing.position

let initial_position filename =
  { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }

let string_of_exn exn =
  let open Objc_lex in
  match exn with
  | UnterminatedBlockComment -> "unterminated block comment"
  | UnterminatedStringLiteral -> "unterminated string"
  | IllegalEscapeSequence -> "invalid escape sequence"
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
      let message = "syntax error" in
      raise @@ ParseError (message, start, end_)
  | I.Accepted v -> v
  | I.Rejected -> assert false

let parse_string ?(filename = "<stdin>") s =
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = filename };
  let lex = Objc_lex.token in
  let checkpoint = Objc_parse.Incremental.file (initial_position filename) in
  loop lex lexbuf checkpoint
