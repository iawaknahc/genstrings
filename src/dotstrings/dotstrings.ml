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

let parse_string ?(filename = "<stdin>") s =
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p
  <- {lexbuf.Lexing.lex_curr_p with pos_fname= filename} ;
  let lex = Dotstrings_lex.new_raw () in
  let checkpoint = Dotstrings_parse.Incremental.plist initial_position in
  loop lex lexbuf checkpoint

let is_printable ch =
  match Uucp.Gc.general_category ch with
  | `Ll | `Lm | `Lo | `Lt | `Lu | `Mc | `Me | `Mn | `Nd | `Nl | `No | `Pc
   |`Pd | `Pe | `Pf | `Pi | `Po | `Ps | `Sc | `Sk | `Sm | `So ->
      true
  | _ -> false

let encode int =
  let i = int - 0x10000 in
  (0xd800 + ((i lsr 10) land 0x3ff), 0xdc00 + (i land 0x3ff))

let write_quoted_string ch s =
  output_char ch '"' ;
  let src = `String s in
  let encoding = `UTF_8 in
  let decoder = Uutf.decoder ~encoding src in
  let buf = Buffer.create 17 in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar ch ->
        let int = Uchar.to_int ch in
        ( match int with
        | 0x5c (* \ *) -> Buffer.add_string buf "\\\\"
        | 0x07 (* bell *) -> Buffer.add_string buf "\\a"
        | 0x08 (* backspace *) -> Buffer.add_string buf "\\b"
        | 0x0c (* form feed *) -> Buffer.add_string buf "\\f"
        | 0x0a (* LF *) -> Buffer.add_string buf "\\n"
        | 0x0d (* CR *) -> Buffer.add_string buf "\\r"
        | 0x09 (* tab *) -> Buffer.add_string buf "\\t"
        | 0x0b (* vertical tab *) -> Buffer.add_string buf "\\v"
        | 0x22 (* double quote *) -> Buffer.add_string buf "\\\""
        | 0x20 (* space *) -> Buffer.add_string buf " "
        | _ ->
            if is_printable ch then Buffer.add_utf_8_uchar buf ch
            else if int < 0x10000 then
              Buffer.add_string buf @@ Printf.sprintf "\\U%04x" int
            else
              let r1, r2 = encode int in
              Buffer.add_string buf @@ Printf.sprintf "\\U%04x\\U%04x" r1 r2 ) ;
        loop ()
    | _ -> ()
  in
  loop () ;
  output_bytes ch (Buffer.to_bytes buf) ;
  output_char ch '"'

let write_channel ch t =
  List.iter
    (fun entry ->
      output_string ch "/* " ;
      output_string ch (String.trim entry.comment) ;
      output_string ch " */\n" ;
      write_quoted_string ch entry.key ;
      output_string ch " = " ;
      write_quoted_string ch entry.value ;
      output_string ch ";\n\n" )
    t
