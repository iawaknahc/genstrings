%{
open Swift_ast
%}

%token EOF
%token <string * Lexing.position> IDENT
%token L_PAREN
%token R_PAREN
%token COMMA
%token COLON
%token ANYTHING_ELSE
%token STRING_START
%token STRING_INTER_START
%token STRING_INTER_END
%token <string> STRING_STATIC
%token STRING_END

%start <Swift_ast.t> file

%%

let file :=
  ~ = list(item); EOF; <>

let item :=
| (ident, pos) = IDENT; { Ident (ident, pos) }
| ANYTHING_ELSE; { Anything_else }
| L_PAREN; { L_paren }
| R_PAREN; { R_paren }
| COMMA; { Comma }
| COLON; { Colon }
| STRING_START; parts = list(string_part); STRING_END; { String parts }

let string_part :=
| string = STRING_STATIC; { StringStatic string }
| STRING_INTER_START; items = list(item); STRING_INTER_END; { StringInterpolation items }
