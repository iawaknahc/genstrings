type token =
  | EOF
  | BareString of string
  | QuotedString of string
  | Bytes of bytes
  | BlockComment of string
  | LineComment of string
  | Semicolon
  | Equal
  | BraceLeft
  | BraceRight
  | ParenLeft
  | ParenRight
  | Comma
