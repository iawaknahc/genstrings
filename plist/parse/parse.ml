module I = Generate.MenhirInterpreter

exception ParseError of string * Lexing.position * Lexing.position

let initial_position =
  {Lexing.pos_fname= "<filename>"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}

let rec loop lex lexbuf checkpoint =
  match checkpoint with
  | I.InputNeeded _ ->
      let result = lex lexbuf in
      let checkpoint = I.offer checkpoint result in
      loop lex lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ -> loop lex lexbuf (I.resume checkpoint)
  | I.HandlingError env ->
      let start, end_ = I.positions env in
      raise @@ ParseError ("SyntaxError", start, end_)
  | I.Accepted v -> v
  | I.Rejected -> assert false

let parse_string s =
  let lexbuf = Lexing.from_string s in
  let lex = Lex.new_raw () in
  let checkpoint = Generate.Incremental.plist initial_position in
  loop lex lexbuf checkpoint
