{
open Swift_parse

exception UnbalancedParenthesis
exception UnterminatedBlockComment
exception UnterminatedString
exception IllegalEscapeSequence

type mode =
| Normal
| RawMultilineString of Lexing.position * int
| RawString of Lexing.position * int
| MultilineString of Lexing.position
| String of Lexing.position

type state = {
  stack: mode Stack.t;
  queue: (token * Lexing.position * Lexing.position) Queue.t;
  buf: Buffer.t;
}

let pop state =
  try Stack.pop state.stack
  with Stack.Empty -> raise UnbalancedParenthesis

let push state mode = Stack.push mode state.stack

let buf state =
  let s = Buffer.contents state.buf in
  Buffer.reset state.buf;
  s

let emit ?start ?end_ state lexbuf token =
  let start = match start with
  | None -> lexbuf.Lexing.lex_start_p
  | Some p -> p
  in
  let end_ = match end_ with
  | None -> lexbuf.Lexing.lex_curr_p
  | Some p -> p
  in
  Queue.add (token, start, end_) state.queue

let mode state =
  try Stack.top state.stack
  with Stack.Empty -> Normal

let replace stack item =
  let _ = Stack.pop stack in
  Stack.push item stack

let next state = Queue.take state.queue

}

let whitespace = [' ' '\t' '\x0b' '\x0c' '\x00']
let linebreak = "\n" | "\r" | "\r\n"

let ident_head = ['a'-'z' 'A'-'Z' '_']
let ident_char = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let ident = ident_head ident_char*

let h = ['0'-'9' 'a'-'f' 'A'-'F']

let unicode_scalar_digits = h | h h | h h h | h h h h | h h h h h | h h h h h h | h h h h h h h | h h h h h h h h

rule token state = parse
| eof { emit state lexbuf EOF }
| whitespace+ { token state lexbuf }
| linebreak { Lexing.new_line lexbuf; token state lexbuf }
| "//" { comment lexbuf; token state lexbuf }
| "/*" { block_comment lexbuf; token state lexbuf }
| '(' {
  push state Normal;
  emit state lexbuf L_PAREN
}
| ')' {
  let start = Lexing.lexeme_end_p lexbuf in
  let _ = pop state in
  (match Stack.top state.stack with
  | Normal -> ()
  | RawMultilineString (_, n) -> replace state.stack (RawMultilineString (start, n))
  | RawString (_, n) -> replace state.stack (RawString (start, n))
  | MultilineString _ -> replace state.stack (MultilineString start)
  | String _ -> replace state.stack (String start)
  | exception Stack.Empty -> ());
  match mode state with
  | Normal -> emit state lexbuf R_PAREN
  | _ -> emit state lexbuf STRING_INTER_END
}
| ':' { emit state lexbuf COLON }
| ',' { emit state lexbuf COMMA }
| ident as s {
  let pos = Lexing.lexeme_start_p lexbuf in
  emit state lexbuf (IDENT (s, pos))
}
| ('#'+ as s) '"' {
  let n = String.length s in
  let p = Lexing.lexeme_end_p lexbuf in
  push state (RawString (p, n));
  emit state lexbuf STRING_START
}
| ('#'+ as s) "\"\"\"" {
  let n = String.length s in
  let p = Lexing.lexeme_end_p lexbuf in
  push state (RawMultilineString (p, n));
  emit state lexbuf STRING_START
}
| '"' {
  let p = Lexing.lexeme_end_p lexbuf in
  push state (String p);
  emit state lexbuf STRING_START
}
| "\"\"\"" {
  let p = Lexing.lexeme_end_p lexbuf in
  push state (MultilineString p);
  emit state lexbuf STRING_START
}
| _ { emit state lexbuf ANYTHING_ELSE }

and comment = parse
| eof { () }
| linebreak { Lexing.new_line lexbuf; }
| _ { comment lexbuf }

and block_comment = parse
| "*/" { () }
| "/*" { block_comment lexbuf; block_comment lexbuf }
| linebreak { Lexing.new_line lexbuf; block_comment lexbuf }
| eof { raise UnterminatedBlockComment }
| _ { block_comment lexbuf }

and raw_string state start len = parse
| eof { raise UnterminatedString }
| linebreak { Lexing.new_line lexbuf; raise UnterminatedString }
| '"' ('#'+ as s) {
  let n = String.length s in
  if n = len
  then (
    let _ = pop state in
    let part = buf state in
    let end_ = Lexing.lexeme_start_p lexbuf in
    emit ~start ~end_ state lexbuf (STRING_STATIC part);
    emit state lexbuf STRING_END
  ) else (
    Buffer.add_string state.buf (Lexing.lexeme lexbuf);
    raw_string state start len lexbuf
  )
}
| _ {
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  raw_string state start len lexbuf
}

and raw_multiline_string state start len = parse
| eof { raise UnterminatedString }
| linebreak {
  Lexing.new_line lexbuf;
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  raw_multiline_string state start len lexbuf
}
| "\"\"\"" ('#'+ as s) {
  let n = String.length s in
  if n = len
  then (
    let _ = pop state in
    let part = buf state in
    let end_ = Lexing.lexeme_start_p lexbuf in
    emit ~start ~end_ state lexbuf (STRING_STATIC part);
    emit state lexbuf STRING_END
  )
  else (
    Buffer.add_string state.buf (Lexing.lexeme lexbuf);
    raw_multiline_string state start len lexbuf
  )
}
| _ {
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  raw_multiline_string state start len lexbuf
}

and string state start = parse
| eof { raise UnterminatedString }
| linebreak { Lexing.new_line lexbuf; raise UnterminatedString }
| '"' {
  let _ = pop state in
  let part = buf state in
  let end_ = Lexing.lexeme_start_p lexbuf in
  emit ~start ~end_ state lexbuf (STRING_STATIC part);
  emit state lexbuf STRING_END
}
| '\\' { raise IllegalEscapeSequence }
| "\\0" { Buffer.add_char state.buf '\000'; string state start lexbuf }
| "\\\\" { Buffer.add_char state.buf '\\'; string state start lexbuf }
| "\\t" { Buffer.add_char state.buf '\t'; string state start lexbuf }
| "\\n" { Buffer.add_char state.buf '\n'; string state start lexbuf }
| "\\r" { Buffer.add_char state.buf '\r'; string state start lexbuf }
| "\\'" { Buffer.add_char state.buf '\''; string state start lexbuf }
| "\\\"" { Buffer.add_char state.buf '"'; string state start lexbuf }
| "\\u{" (unicode_scalar_digits as s) '}' {
  let i = int_of_string ("0x" ^ s) in
  let u = try Uchar.of_int i with Invalid_argument _ -> raise IllegalEscapeSequence in
  Buffer.add_utf_8_uchar state.buf u;
  string state start lexbuf
}
| "\\(" {
  push state Normal;
  let part = buf state in
  let end_ = Lexing.lexeme_start_p lexbuf in
  emit ~start ~end_ state lexbuf (STRING_STATIC part);
  emit state lexbuf STRING_INTER_START
}
| _ { Buffer.add_string state.buf (Lexing.lexeme lexbuf); string state start lexbuf }

and multiline_string state start = parse
| eof { raise UnterminatedString }
| linebreak {
  Lexing.new_line lexbuf;
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  multiline_string state start lexbuf
}
| "\"\"\"" {
  let _ = pop state in
  let part = buf state in
  let end_ = Lexing.lexeme_start_p lexbuf in
  emit ~start ~end_ state lexbuf (STRING_STATIC part);
  emit state lexbuf STRING_END
}
| '\\' { raise IllegalEscapeSequence }
| "\\0" { Buffer.add_char state.buf '\000'; multiline_string state start lexbuf }
| "\\\\" { Buffer.add_char state.buf '\\'; multiline_string state start lexbuf }
| "\\t" { Buffer.add_char state.buf '\t'; multiline_string state start lexbuf }
| "\\n" { Buffer.add_char state.buf '\n'; multiline_string state start lexbuf }
| "\\r" { Buffer.add_char state.buf '\r'; multiline_string state start lexbuf }
| "\\'" { Buffer.add_char state.buf '\''; multiline_string state start lexbuf }
| "\\\"" { Buffer.add_char state.buf '"'; multiline_string state start lexbuf }
| "\\u{" (unicode_scalar_digits as s) '}' {
  let i = int_of_string ("0x" ^ s) in
  let u = try Uchar.of_int i with Invalid_argument _ -> raise IllegalEscapeSequence in
  Buffer.add_utf_8_uchar state.buf u;
  multiline_string state start lexbuf
}
| "\\(" {
  push state Normal;
  let part = buf state in
  let end_ = Lexing.lexeme_start_p lexbuf in
  emit ~start ~end_ state lexbuf (STRING_STATIC part);
  emit state lexbuf STRING_INTER_START
}
| '\\' whitespace* linebreak {
  Lexing.new_line lexbuf;
  multiline_string state start lexbuf
}
| _ {
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  multiline_string state start lexbuf
}

{
let make () =
  let state = {
    stack = Stack.create ();
    queue = Queue.create ();
    buf = Buffer.create 17;
  } in
  fun lexbuf -> (
    let () = match mode state with
    | Normal -> token state lexbuf
    | String start -> string state start lexbuf
    | MultilineString start -> multiline_string state start lexbuf
    | RawString (start, n) -> raw_string state start n lexbuf
    | RawMultilineString (start, n) -> raw_multiline_string state start n lexbuf
    in
    next state
  )
}
