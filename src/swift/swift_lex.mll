{
open Swift_parse

exception UnbalancedParenthesis
exception UnterminatedBlockComment
exception UnterminatedString
exception IllegalEscapeSequence
exception IllegalMultilineStringBegin
exception IllegalMultilineStringEnd
exception InsufficientMultilineStringIndentation

type mode =
| Normal
| RawMultilineString of int
| RawString of int
| MultilineString
| String

type state = {
  stack: mode Stack.t;
  queue: token Queue.t;
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

let emit state token = Queue.add token state.queue

let mode state =
  try Stack.top state.stack
  with Stack.Empty -> Normal

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
| eof { emit state EOF }
| whitespace+ { token state lexbuf }
| linebreak { Lexing.new_line lexbuf; token state lexbuf }
| "//" { comment lexbuf; token state lexbuf }
| "/*" { block_comment lexbuf; token state lexbuf }
| '(' {
  push state Normal;
  emit state L_PAREN
}
| ')' {
  let _ = pop state in
  match mode state with
  | Normal -> emit state R_PAREN
  | _ -> emit state STRING_INTER_END
}
| ':' { emit state COLON }
| ',' { emit state COMMA }
| ident as s {
  let pos = Lexing.lexeme_start_p lexbuf in
  emit state (IDENT (s, pos))
}
| ('#'+ as s) '"' {
  push state (RawString (String.length s));
  emit state STRING_START
}
| ('#'+ as s) "\"\"\"" {
  push state (RawMultilineString (String.length s));
  emit state STRING_START
}
| '"' {
  push state String;
  emit state STRING_START
}
| "\"\"\"" {
  push state MultilineString;
  emit state STRING_START
}
| _ { emit state ANYTHING_ELSE }

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

and raw_string state len = parse
| eof { raise UnterminatedString }
| linebreak { Lexing.new_line lexbuf; raise UnterminatedString }
| '"' ('#'+ as s) {
  let n = String.length s in
  if n = len
  then (
    let _ = pop state in
    let part = buf state in
    emit state (STRING_STATIC part);
    emit state STRING_END
  ) else (
    Buffer.add_string state.buf (Lexing.lexeme lexbuf);
    raw_string state len lexbuf
  )
}
| _ {
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  raw_string state len lexbuf
}

and raw_multiline_string state len = parse
| eof { raise UnterminatedString }
| linebreak {
  Lexing.new_line lexbuf;
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  raw_multiline_string state len lexbuf
}
| "\"\"\"" ('#'+ as s) {
  let n = String.length s in
  if n = len
  then (
    let _ = pop state in
    let part = buf state in
    emit state (STRING_STATIC part);
    emit state STRING_END
  )
  else (
    Buffer.add_string state.buf (Lexing.lexeme lexbuf);
    raw_multiline_string state len lexbuf
  )
}
| _ {
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  raw_multiline_string state len lexbuf
}

and string state = parse
| eof { raise UnterminatedString }
| linebreak { Lexing.new_line lexbuf; raise UnterminatedString }
| '"' {
  let _ = pop state in
  let part = buf state in
  emit state (STRING_STATIC part);
  emit state STRING_END
}
| '\\' { raise IllegalEscapeSequence }
| "\\0" { Buffer.add_char state.buf '\000'; string state lexbuf }
| "\\\\" { Buffer.add_char state.buf '\\'; string state lexbuf }
| "\\t" { Buffer.add_char state.buf '\t'; string state lexbuf }
| "\\n" { Buffer.add_char state.buf '\n'; string state lexbuf }
| "\\r" { Buffer.add_char state.buf '\r'; string state lexbuf }
| "\\'" { Buffer.add_char state.buf '\''; string state lexbuf }
| "\\\"" { Buffer.add_char state.buf '"'; string state lexbuf }
| "\\u{" (unicode_scalar_digits as s) '}' {
  let i = int_of_string ("0x" ^ s) in
  let u = try Uchar.of_int i with Invalid_argument _ -> raise IllegalEscapeSequence in
  Buffer.add_utf_8_uchar state.buf u;
  string state lexbuf
}
| "\\(" {
  push state Normal;
  let part = buf state in
  emit state (STRING_STATIC part);
  emit state STRING_INTER_START
}
| _ { Buffer.add_string state.buf (Lexing.lexeme lexbuf); string state lexbuf }

and multiline_string state = parse
| eof { raise UnterminatedString }
| linebreak {
  Lexing.new_line lexbuf;
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  multiline_string state lexbuf
}
| "\"\"\"" {
  let _ = pop state in
  let part = buf state in
  emit state (STRING_STATIC part);
  emit state STRING_END
}
| '\\' { raise IllegalEscapeSequence }
| "\\0" { Buffer.add_char state.buf '\000'; multiline_string state lexbuf }
| "\\\\" { Buffer.add_char state.buf '\\'; multiline_string state lexbuf }
| "\\t" { Buffer.add_char state.buf '\t'; multiline_string state lexbuf }
| "\\n" { Buffer.add_char state.buf '\n'; multiline_string state lexbuf }
| "\\r" { Buffer.add_char state.buf '\r'; multiline_string state lexbuf }
| "\\'" { Buffer.add_char state.buf '\''; multiline_string state lexbuf }
| "\\\"" { Buffer.add_char state.buf '"'; multiline_string state lexbuf }
| "\\u{" (unicode_scalar_digits as s) '}' {
  let i = int_of_string ("0x" ^ s) in
  let u = try Uchar.of_int i with Invalid_argument _ -> raise IllegalEscapeSequence in
  Buffer.add_utf_8_uchar state.buf u;
  multiline_string state lexbuf
}
| "\\(" {
  push state Normal;
  let part = buf state in
  emit state (STRING_STATIC part);
  emit state STRING_INTER_START
}
| '\\' whitespace* linebreak {
  Lexing.new_line lexbuf;
  multiline_string state lexbuf
}
| _ {
  Buffer.add_string state.buf (Lexing.lexeme lexbuf);
  multiline_string state lexbuf
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
    | String -> string state lexbuf
    | MultilineString -> multiline_string state lexbuf
    | RawString n -> raw_string state n lexbuf
    | RawMultilineString n -> raw_multiline_string state n lexbuf
    in
    next state
  )
}
