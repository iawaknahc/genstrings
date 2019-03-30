{
open Swift_token
open Swift_lex_header
}

let whitespace = [' ' '\t' '\x0b' '\x0c' '\x00']
let linebreak = "\n" | "\r" | "\r\n"

let ident_head = ['a'-'z' 'A'-'Z' '_']
let ident_char = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let ident = ident_head ident_char*

let h = ['0'-'9' 'a'-'f' 'A'-'F']

let unicode_scalar_digits = h | h h | h h h | h h h h | h h h h h | h h h h h h | h h h h h h h | h h h h h h h h

rule token = parse
| eof { Eof }
| whitespace+ { token lexbuf }
| linebreak { Lexing.new_line lexbuf; token lexbuf }
| "//" { comment lexbuf; token lexbuf }
| "/*" { block_comment lexbuf; token lexbuf }
| '(' { ParenLeft }
| ')' { ParenRight }
| ':' { Colon }
| ',' { Comma }
| ident as s { Ident s }
| ('#'+ as s) '"' { raw_string (Buffer.create 17) (String.length s) lexbuf }
| ('#'+ as s) "\"\"\"" { raw_multiline_string [] (Buffer.create 17) (String.length s) lexbuf }
| '"' { static_string (Buffer.create 17) lexbuf }
| "\"\"\"" { static_multiline_string [] (Buffer.create 17) lexbuf }
| _ { token lexbuf }

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

and raw_string buf len = parse
| eof { raise UnterminatedString }
| linebreak { Lexing.new_line lexbuf; raise UnterminatedString }
| '"' ('#'+ as s) {
  let n = String.length s in
  if n = len
  then
    String (Buffer.contents buf)
  else (
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    raw_string buf len lexbuf
  )
}
| _ {
  Buffer.add_string buf (Lexing.lexeme lexbuf);
  raw_string buf len lexbuf
}

and raw_multiline_string lines buf len = parse
| eof { raise UnterminatedString }
| linebreak {
  Lexing.new_line lexbuf;
  let line = Line (Buffer.contents buf) in
  raw_multiline_string (line::lines) (Buffer.create 17) len lexbuf
}
| "\"\"\"" ('#'+ as s) {
  let n = String.length s in
  if n = len
  then (
    let line = Line (Buffer.contents buf) in
    let reverted_lines = line::lines in
    String (unindent reverted_lines)
  )
  else (
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    raw_multiline_string lines buf len lexbuf
  )
}
| _ {
  Buffer.add_string buf (Lexing.lexeme lexbuf);
  raw_multiline_string lines buf len lexbuf
}

and static_string buf = parse
| eof { raise UnterminatedString }
| linebreak { Lexing.new_line lexbuf; raise UnterminatedString }
| '"' { String (Buffer.contents buf) }
| '\\' { raise IllegalEscapeSequence }
| "\\0" { Buffer.add_char buf '\000'; static_string buf lexbuf }
| "\\\\" { Buffer.add_char buf '\\'; static_string buf lexbuf }
| "\\t" { Buffer.add_char buf '\t'; static_string buf lexbuf }
| "\\n" { Buffer.add_char buf '\n'; static_string buf lexbuf }
| "\\r" { Buffer.add_char buf '\r'; static_string buf lexbuf }
| "\\'" { Buffer.add_char buf '\''; static_string buf lexbuf }
| "\\\"" { Buffer.add_char buf '"'; static_string buf lexbuf }
| "\\u{" (unicode_scalar_digits as s) '}' {
  let i = int_of_string ("0x" ^ s) in
  let u = try Uchar.of_int i with Invalid_argument _ -> raise IllegalEscapeSequence in
  Buffer.add_utf_8_uchar buf u;
  static_string buf lexbuf
}
| _ { Buffer.add_string buf (Lexing.lexeme lexbuf); static_string buf lexbuf }

and static_multiline_string lines buf = parse
| eof { raise UnterminatedString }
| linebreak {
  Lexing.new_line lexbuf;
  let line = Line (Buffer.contents buf) in
  static_multiline_string (line::lines) (Buffer.create 17) lexbuf
}
| "\"\"\"" {
  let line = Line (Buffer.contents buf) in
  let reverted_lines = line::lines in
  String (unindent reverted_lines)
}
| '\\' { raise IllegalEscapeSequence }
| "\\0" { Buffer.add_char buf '\000'; static_multiline_string lines buf lexbuf }
| "\\\\" { Buffer.add_char buf '\\'; static_multiline_string lines buf lexbuf }
| "\\t" { Buffer.add_char buf '\t'; static_multiline_string lines buf lexbuf }
| "\\n" { Buffer.add_char buf '\n'; static_multiline_string lines buf lexbuf }
| "\\r" { Buffer.add_char buf '\r'; static_multiline_string lines buf lexbuf }
| "\\'" { Buffer.add_char buf '\''; static_multiline_string lines buf lexbuf }
| "\\\"" { Buffer.add_char buf '"'; static_multiline_string lines buf lexbuf }
| "\\u{" (unicode_scalar_digits as s) '}' {
  let i = int_of_string ("0x" ^ s) in
  let u = try Uchar.of_int i with Invalid_argument _ -> raise IllegalEscapeSequence in
  Buffer.add_utf_8_uchar buf u;
  static_multiline_string lines buf lexbuf
}
| '\\' whitespace* linebreak {
  Lexing.new_line lexbuf;
  let line = EscapedLine (Buffer.contents buf) in
  static_multiline_string (line::lines) (Buffer.create 17) lexbuf
}
| _ { Buffer.add_string buf (Lexing.lexeme lexbuf); static_multiline_string lines buf lexbuf }
