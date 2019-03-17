{
type raw_token =
| EOF
| BareString of string
| QuotedString of string
| Bytes of bytes
| BlockComment of string
| LineComment of string
| Semicolon
| Equal
| BraceLeft
| BraceRight
| ParenLeft
| ParenRight
| Comma

type state = {
  mutable bare_string_start : Lexing.position option;
  mutable slash_start : Lexing.position option;
  queue : (raw_token * Lexing.position * Lexing.position) Queue.t;
  buf : Buffer.t;
}

type utf16 =
| SingleCodeUnit
| HighSurrogate
| LowSurrogate

exception UnterminatedBlockComment
exception UnterminatedStringLiteral
exception UnterminatedBytes
exception InvalidEscapeSequence
exception InvalidCharacter of char

let surr1 = 0xd800
let surr2 = 0xdc00
let surr3 = 0xe000
let surr_self = 0x10000

let classify code_unit =
  if code_unit < surr1
  then SingleCodeUnit
  else if code_unit < surr2
  then HighSurrogate
  else if code_unit < surr3
  then LowSurrogate
  else failwith "unreachable"

let utf16_high_low_to_code_point high low =
  let a = (high - surr1) lsl 10 in
  let b = (low - surr1) + surr_self in
  a lor b

let hex_to_char a b =
  let code c = match c with
  | '0'..'9' -> Char.code c - 48
  | 'A'..'F' -> Char.code c - 55
  | 'a'..'f' -> Char.code c - 87
  | _ -> failwith "unreachable"
  in
  Char.chr (code a lsl 4 + code b)

let hex_to_bytes hex =
  if hex = "" then Bytes.empty
  else
    let len = String.length hex in
    let buf = Bytes.create (len / 2) in
    let rec loop i j =
      if i >= len then ()
      else (
        Bytes.set buf (i / 2) (hex_to_char hex.[i] hex.[j]);
        loop (j + 1) (j + 2)
      )
    in loop 0 1;
    buf

let new_state () = {
  bare_string_start = None;
  slash_start = None;
  buf = Buffer.create 17;
  queue = Queue.create ();
}

let remove_slash state =
  let len = Buffer.length state.buf in
  Buffer.truncate state.buf (len - 1)

let flush ?end_ state lexbuf =
  state.slash_start <- None;
  match state.bare_string_start with
  | None -> ()
  | Some start ->
    state.bare_string_start <- None;
    let end_ = match end_ with
    | None -> lexbuf.Lexing.lex_start_p
    | Some end_ -> end_
    in
    let t = BareString (Buffer.contents state.buf) in
    Buffer.clear state.buf;
    Queue.add (t, start, end_) state.queue

let mark state lexbuf =
  match state.bare_string_start with
  | None ->
    state.bare_string_start <- Some lexbuf.Lexing.lex_start_p
  | Some _ -> ()

let save ?start ?end_ state lexbuf t =
  let start = match start with
  | None -> lexbuf.Lexing.lex_start_p
  | Some start -> start
  in
  let end_ = match end_ with
  | None -> lexbuf.Lexing.lex_curr_p
  | Some end_ -> end_
  in
  Queue.add (t, start, end_) state.queue;
  Queue.take state.queue
}

let whitespace = [' ' '\t']
let newline = "\n" | "\r\n"
let bare_string = ['a'-'z' 'A'-'Z' '0'-'9' '$' '-' '_' '.' ':']
let octal = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

rule lex state = parse
| eof { flush state lexbuf; save state lexbuf EOF }
| ';' { flush state lexbuf; save state lexbuf Semicolon }
| '=' { flush state lexbuf; save state lexbuf Equal }
| '{' { flush state lexbuf; save state lexbuf BraceLeft }
| '}' { flush state lexbuf; save state lexbuf BraceRight}
| '(' { flush state lexbuf; save state lexbuf ParenLeft }
| ')' { flush state lexbuf; save state lexbuf ParenRight }
| ',' { flush state lexbuf; save state lexbuf Comma }
| newline {
  flush state lexbuf;
  Lexing.new_line lexbuf;
  lex state lexbuf
}
| whitespace+ { flush state lexbuf; lex state lexbuf }
| '"' {
  let start = lexbuf.Lexing.lex_start_p in
  flush state lexbuf;
  let quoted_string = lex_quoted_string (Buffer.create 17) lexbuf in
  save ~start state lexbuf quoted_string
}
| '<' {
  let start = lexbuf.Lexing.lex_start_p in
  flush state lexbuf;
  let bytes = lex_bytes (Buffer.create 17) lexbuf in
  save ~start state lexbuf bytes
}
| "/*" {
  let start = lexbuf.Lexing.lex_start_p in
  flush state lexbuf;
  let block_comment = lex_block_comment (Buffer.create 17) lexbuf in
  save ~start state lexbuf block_comment
}
| bare_string+ as s {
  state.slash_start <- None;
  mark state lexbuf;
  Buffer.add_string state.buf s;
  lex state lexbuf
}
| '/' as ch {
  match state.slash_start with
  | None ->
    (* Remember it is the first time we see a slash *)
    state.slash_start <- Some lexbuf.Lexing.lex_start_p;
    (* Mark this is the start of a bare string if needed *)
    mark state lexbuf;
    Buffer.add_char state.buf ch;
    lex state lexbuf
  | Some p ->
    (* We see // *)
    (* So the first slash should not be part of the bare string *)
    remove_slash state;
    (
      (* If bare_string_start is slash_start then there is no bare string *)
      match state.bare_string_start with
      | None -> ()
      | Some bare_string_start ->
        if p = bare_string_start
        then state.bare_string_start <- None
    );

    (* Possibly flush the preceding bare string which ends with slash_start *)
    flush ~end_:p state lexbuf;

    let line_comment = lex_line_comment (Buffer.create 17) lexbuf in
    save ~start:p state lexbuf line_comment
}
| _ as ch { flush state lexbuf; raise (InvalidCharacter ch) }

and lex_quoted_string buf = parse
| eof | '\n' | '\r' { raise UnterminatedStringLiteral }
| '"' { QuotedString (Buffer.contents buf) }
| '\\' '\\' { Buffer.add_char buf '\\'; lex_quoted_string buf lexbuf }
| '\\' 'a' { Buffer.add_char buf '\x07'; lex_quoted_string buf lexbuf }
| '\\' 'b' { Buffer.add_char buf '\b'; lex_quoted_string buf lexbuf }
| '\\' 'f' { Buffer.add_char buf '\x0c'; lex_quoted_string buf lexbuf }
| '\\' 'n' { Buffer.add_char buf '\n'; lex_quoted_string buf lexbuf }
| '\\' 'r' { Buffer.add_char buf '\r'; lex_quoted_string buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; lex_quoted_string buf lexbuf }
| '\\' 'v' { Buffer.add_char buf '\x0b'; lex_quoted_string buf lexbuf }
| '\\' '\'' { Buffer.add_char buf '\''; lex_quoted_string buf lexbuf }
| '\\' '"' { Buffer.add_char buf '"'; lex_quoted_string buf lexbuf }
| '\\' '?' { Buffer.add_char buf '?'; lex_quoted_string buf lexbuf }
| '\\' (octal octal octal as octal) {
  let i = int_of_string ("0o" ^ octal) in
  let c = Char.chr i in
  Buffer.add_char buf c;
  lex_quoted_string buf lexbuf
}
| '\\' 'U' { lex_utf16 buf lexbuf }
| '\\' eof { raise InvalidEscapeSequence }
| '\\' _ { raise InvalidEscapeSequence }
| _ as ch { Buffer.add_char buf ch; lex_quoted_string buf lexbuf }

and lex_utf16 buf = parse
| hex | hex hex | hex hex hex | hex hex hex hex as hex {
  let code_unit = int_of_string ("0x" ^ hex) in
  match classify code_unit with
  | SingleCodeUnit -> (
    Buffer.add_utf_8_uchar buf (Uchar.of_int code_unit);
    lex_quoted_string buf lexbuf
  )
  | HighSurrogate -> lex_utf16_low buf code_unit lexbuf
  | LowSurrogate -> raise InvalidEscapeSequence
}
| eof | _ { raise InvalidEscapeSequence }

and lex_utf16_low buf high = parse
| '\\' 'U' ((hex | hex hex | hex hex hex | hex hex hex hex) as hex) {
  let code_unit = int_of_string ("0x" ^ hex) in
  match classify code_unit with
  | LowSurrogate -> (
    let code_point = utf16_high_low_to_code_point high code_unit in
    Buffer.add_utf_8_uchar buf (Uchar.of_int code_point);
    lex_quoted_string buf lexbuf
  )
  | _ -> raise InvalidEscapeSequence
}
| eof | _ { raise InvalidEscapeSequence }

and lex_line_comment buf = parse
| eof { LineComment (Buffer.contents buf) }
| newline { Lexing.new_line lexbuf; LineComment (Buffer.contents buf) }
| _ as ch { Buffer.add_char buf ch; lex_line_comment buf lexbuf }

and lex_block_comment buf = parse
| eof { raise UnterminatedBlockComment }
| "*/" { BlockComment (Buffer.contents buf) }
| newline as s {
  Lexing.new_line lexbuf;
  Buffer.add_string buf s;
  lex_block_comment buf lexbuf
}
| _ as ch { Buffer.add_char buf ch; lex_block_comment buf lexbuf }

and lex_bytes buf = parse
| newline { Lexing.new_line lexbuf; lex_bytes buf lexbuf }
| whitespace+ { lex_bytes buf lexbuf }
| '>' { Bytes (Buffer.to_bytes buf) }
| (hex hex)* as hex {
  let bytes = hex_to_bytes hex in
  Buffer.add_bytes buf bytes;
  lex_bytes buf lexbuf
}
| eof | _ { raise UnterminatedBytes }
{
open Token

let new_raw () =
  let state = new_state () in
  fun lexbuf -> lex state lexbuf

let new_lex () =
  let lex = new_raw () in
  let rev = List.rev in
  fun lexbuf ->
    let rec loop (comments: comment list) =
      match lex lexbuf with
      | (BlockComment comment, _ , _) -> loop @@ BlockComment comment :: comments
      | (LineComment comment, _, _) -> loop @@ LineComment comment :: comments
      | (EOF, s, e) -> EOF (rev comments), s, e
      | (BareString a, s, e) -> BareString (a, rev comments), s, e
      | (QuotedString a, s, e) -> QuotedString (a, rev comments), s, e
      | (Bytes a, s, e) -> Bytes (a, rev comments), s, e
      | (Semicolon, s, e) -> Semicolon (rev comments), s, e
      | (Equal, s, e) -> Equal (rev comments), s, e
      | (BraceLeft, s, e) -> BraceLeft (rev comments), s, e
      | (BraceRight, s, e) -> BraceRight (rev comments), s, e
      | (ParenLeft, s, e) -> ParenLeft (rev comments), s, e
      | (ParenRight, s, e) -> ParenRight (rev comments), s, e
      | (Comma, s, e) -> Comma (rev comments), s, e
    in
    loop []
}
