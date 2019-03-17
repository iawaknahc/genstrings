{
type t =
| EOF
| BareString of string
| QuotedString of string
| Bytes of bytes
| Comment of string
| Semicolon
| Equal
| BraceLeft
| BraceRight
| ParenLeft
| ParenRight
| Comma

type state = {
  mutable last_char_is_slash: bool;
  queue: t Queue.t;
  buf: Buffer.t;
}

type utf16 =
| SingleCodeUnit
| HighSurrogate
| LowSurrogate

exception UnterminatedComment
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
  last_char_is_slash = false;
  buf = Buffer.create 17;
  queue = Queue.create ();
}

let remove_slash state =
  let len = Buffer.length state.buf in
  Buffer.truncate state.buf (len - 1)
;;

let flush state =
  state.last_char_is_slash <- false;
  let len = Buffer.length state.buf in
  if len <> 0 then (
    let tt = BareString (Buffer.contents state.buf) in
    Buffer.clear state.buf;
    Queue.add tt state.queue;
  )
;;

let save state t =
  Queue.add t state.queue;
  Queue.take state.queue
}

let whitespace = [' ' '\t' '\n' '\r']
let newline = "\n" | "\r\n"
let bare_string = ['a'-'z' 'A'-'Z' '0'-'9' '$' '-' '_' '.' ':']
let octal = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

rule lex_with_comment state = parse
| eof { flush state; save state EOF }
| ';' { flush state; save state Semicolon }
| '=' { flush state; save state Equal }
| '{' { flush state; save state BraceLeft }
| '}' { flush state; save state BraceRight }
| '(' { flush state; save state ParenLeft }
| ')' { flush state; save state ParenRight }
| ',' { flush state; save state Comma }
| '/' as ch {
  if state.last_char_is_slash
  then (
    remove_slash state;
    flush state;
    let line_comment = lex_line_comment (Buffer.create 17) lexbuf in
    save state line_comment
  )
  else (
    state.last_char_is_slash <- true;
    Buffer.add_char state.buf ch;
    lex_with_comment state lexbuf
  )
}
| bare_string+ as s {
  state.last_char_is_slash <- false;
  Buffer.add_string state.buf s;
  lex_with_comment state lexbuf
}
| whitespace+ { flush state; lex_with_comment state lexbuf }
| '"' {
  flush state;
  let quoted_string = lex_quoted_string (Buffer.create 17) lexbuf in
  save state quoted_string
}
| '<' {
  flush state;
  let bytes = lex_bytes (Buffer.create 17) lexbuf in
  save state bytes
}
| "/*" {
  flush state;
  let block_comment = lex_block_comment (Buffer.create 17) lexbuf in
  save state block_comment
}
| _ as ch { flush state; raise (InvalidCharacter ch) }

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
| eof | newline { Comment (Buffer.contents buf) }
| _ as ch { Buffer.add_char buf ch; lex_line_comment buf lexbuf }

and lex_block_comment buf = parse
| eof { raise UnterminatedComment }
| "*/" { Comment (Buffer.contents buf) }
| _ as ch { Buffer.add_char buf ch; lex_block_comment buf lexbuf }

and lex_bytes buf = parse
| whitespace+ { lex_bytes buf lexbuf }
| '>' { Bytes (Buffer.to_bytes buf) }
| (hex hex)* as hex {
  let bytes = hex_to_bytes hex in
  Buffer.add_bytes buf bytes;
  lex_bytes buf lexbuf
}
| eof | _ { raise UnterminatedBytes }
{
let new_lex () =
  let state = new_state () in
  fun lexbuf -> lex_with_comment state lexbuf
}
