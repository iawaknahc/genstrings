include Swift_ast
module I = Swift_parse.MenhirInterpreter

exception ParseError of string * Lexing.position * Lexing.position

let initial_position =
  {Lexing.pos_fname= ""; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}

let string_of_exn exn =
  let open Swift_lex in
  match exn with
  | UnbalancedParenthesis -> "unbalanced parenthesis"
  | UnterminatedBlockComment -> "unterminated block comment"
  | UnterminatedString -> "unterminated string"
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

let parse_string s =
  let lexbuf = Lexing.from_string s in
  let lex = Swift_lex.make () in
  let checkpoint = Swift_parse.Incremental.file initial_position in
  loop lex lexbuf checkpoint
