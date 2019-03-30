exception UnterminatedBlockComment
exception UnterminatedString
exception IllegalEscapeSequence
exception IllegalMultilineStringBegin
exception IllegalMultilineStringEnd
exception InsufficientMultilineStringIndentation

type multiline = Line of string | EscapedLine of string

let line_content = function Line s -> s | EscapedLine s -> s

let ensure_indent line =
  let check_indent s =
    let len = String.length s in
    for i = 0 to len - 1 do
      let ch = s.[i] in
      if
        ch <> ' ' && ch <> '\t' && ch <> '\x0b' && ch <> '\x0c' && ch <> '\x00'
      then raise IllegalMultilineStringEnd
    done ;
    s
  in
  check_indent (line_content line)

let trim_prefix prefix s =
  if s = "" then s
  else
    let prefix_len = String.length prefix in
    let len = String.length s in
    if prefix_len > len then raise InsufficientMultilineStringIndentation
    else
      try
        for i = 0 to prefix_len - 1 do
          if s.[i] <> prefix.[i] then raise Exit
        done ;
        String.sub s prefix_len (len - prefix_len)
      with Exit -> raise InsufficientMultilineStringIndentation

let join_lines lines =
  let buf = Buffer.create 17 in
  let rec loop = function
    | Line s :: rest ->
        Buffer.add_string buf s ;
        (match rest with _ :: _ -> Buffer.add_char buf '\n' | _ -> ()) ;
        loop rest
    | EscapedLine s :: rest -> Buffer.add_string buf s ; loop rest
    | [] -> Buffer.contents buf
  in
  loop lines

let unindent reverted_lines =
  match reverted_lines with
  (* Ensure multiline string has end delimiter on its own line *)
  | line :: reverted_lines -> (
      (* Ensure indent contains whitespace only *)
      let indent = ensure_indent line in
      let lines = List.rev reverted_lines in
      match lines with
      | line :: lines ->
          (* Ensure the first line is empty *)
          let _ =
            try assert (line_content line = "")
            with _ -> raise IllegalMultilineStringBegin
          in
          (* Verify every non blank line has the same indent *)
          lines
          |> List.map (fun line ->
                 match line with
                 | Line s -> Line (trim_prefix indent s)
                 | EscapedLine s -> EscapedLine (trim_prefix indent s) )
          |> join_lines
      | _ -> raise IllegalMultilineStringBegin )
  | _ -> raise IllegalMultilineStringEnd
