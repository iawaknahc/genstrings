%{
open Objc_ast
%}

%token EOF
%token <string * Lexing.position> IDENT
%token <string> STRING
%token L_PAREN
%token R_PAREN
%token COMMA
%token AT
%token ANYTHING_ELSE

%start <Objc_ast.t> file

%%

let file :=
  ~ = list(item); EOF; <>

let item :=
| (ident, pos) = IDENT; { Ident (ident, pos) }
| ANYTHING_ELSE; { Anything_else }
| L_PAREN; { L_paren }
| R_PAREN; { R_paren }
| COMMA; { Comma }
| AT; { At }
| s = STRING; { String s }
