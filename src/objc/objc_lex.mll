{
open Objc_parse

exception UnterminatedBlockComment
exception UnterminatedStringLiteral
exception IllegalEscapeSequence

let emit ?start ?end_ lexbuf token =
  let start = match start with
  | Some p -> p
  | None -> lexbuf.Lexing.lex_start_p
  in
  let end_ = match end_ with
  | Some p -> p
  | None -> lexbuf.Lexing.lex_curr_p
  in
  (token, start, end_)
}

(* C11 6.4 3 Definition of whitespace *)
let whitespace = [' ' '\t' '\x0b' '\x0c']
let newline = "\n" | "\r\n"

let ident_head = ['a'-'z' 'A'-'Z' '_']
let ident_char = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let ident = ident_head ident_char*

let octal = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

rule token = parse
| eof { emit lexbuf EOF }
| newline { Lexing.new_line lexbuf; token lexbuf }
| ident as s {
  let pos = Lexing.lexeme_start_p lexbuf in
  emit lexbuf (IDENT (s, pos))
}
| whitespace+ { token lexbuf }
| "/*" { block_comment lexbuf; token lexbuf }
| "//" { line_comment lexbuf; token lexbuf }
| '(' { emit lexbuf L_PAREN }
| ')' { emit lexbuf R_PAREN }
| '@' { emit lexbuf AT }
| ',' { emit lexbuf COMMA }
| '"' {
  let start = lexbuf.Lexing.lex_start_p in
  string (Buffer.create 17) start lexbuf
}
| _ { emit lexbuf ANYTHING_ELSE }

and string buf start = parse
| eof { raise UnterminatedStringLiteral }
| newline { Lexing.new_line lexbuf; raise UnterminatedStringLiteral }
| '"' { emit ~start lexbuf (STRING (Buffer.contents buf)) }
| "\\'" { Buffer.add_char buf '\''; string buf start lexbuf }
| "\\\"" { Buffer.add_char buf '"'; string buf start lexbuf }
| "\\?" { Buffer.add_char buf '?'; string buf start lexbuf }
| "\\\\" { Buffer.add_char buf '\\'; string buf start lexbuf }
| "\\a" { Buffer.add_char buf '\x07'; string buf start lexbuf }
| "\\b" { Buffer.add_char buf '\b'; string buf start lexbuf }
| "\\f" { Buffer.add_char buf '\x0c'; string buf start lexbuf }
| "\\n" { Buffer.add_char buf '\n'; string buf start lexbuf }
| "\\r" { Buffer.add_char buf '\r'; string buf start lexbuf }
| "\\t" { Buffer.add_char buf '\t'; string buf start lexbuf }
| "\\v" { Buffer.add_char buf '\x0b'; string buf start lexbuf }
| '\\' ((octal | octal octal | octal | octal | octal) as s) {
  let i = int_of_string ("0o" ^ s) in
  let c = Char.chr i in
  Buffer.add_char buf c;
  string buf start lexbuf
}
| "\\u" (hex hex hex hex as s) {
  let i = int_of_string ("0x" ^ s) in
  let u = try Uchar.of_int i with Invalid_argument _ -> raise IllegalEscapeSequence in
  Buffer.add_utf_8_uchar buf u;
  string buf start lexbuf
}
| "\\U" (hex hex hex hex hex hex hex hex as s) {
  let i = int_of_string ("0x" ^ s) in
  let u = try Uchar.of_int i with Invalid_argument _ -> raise IllegalEscapeSequence in
  Buffer.add_utf_8_uchar buf u;
  string buf start lexbuf
}
| "\\x" ((hex | hex hex ) as s) {
  let i = int_of_string ("0x" ^ s) in
  let c = Char.chr i in
  Buffer.add_char buf c;
  string buf start lexbuf
}
| '\\' _ { raise IllegalEscapeSequence }
| _ { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf start lexbuf }

and line_comment = parse
| eof { () }
| newline { Lexing.new_line lexbuf; }
| _ { line_comment lexbuf }

and block_comment = parse
| eof { raise UnterminatedBlockComment }
| "*/" { () }
| newline { Lexing.new_line lexbuf; block_comment lexbuf }
| _ { block_comment lexbuf }
