exception UnbalancedParenthesis
exception UnterminatedBlockComment
exception UnterminatedString
exception IllegalEscapeSequence

val make : unit -> Lexing.lexbuf -> Swift_parse.token
