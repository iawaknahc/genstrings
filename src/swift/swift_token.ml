type token =
  | Eof
  | Ident of string
  | String of string
  | ParenLeft
  | ParenRight
  | Colon
  | Comma
