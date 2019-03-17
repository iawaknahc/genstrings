%token EOF
%token <string> BareString
%token <string> QuotedString
%token <bytes> Bytes
%token <string> BlockComment
%token <string> LineComment
%token Semicolon
%token Equal
%token BraceLeft
%token BraceRight
%token ParenLeft
%token ParenRight
%token Comma

%start <Ast.t> plist

%%

let plist :=
  ~ = list(entry); EOF; <>

let entry :=
  comment = BlockComment; key = string; Equal; value = string; Semicolon;
  { { Ast.comment; key; value; } }

let string :=
| ~ = BareString; <>
| ~ = QuotedString; <>
