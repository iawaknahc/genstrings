type routine_call = {key: string; comment: string; pos: Lexing.position}

let collect_swift ~filename ~routine_name queue ast =
  let open Swift in
  let rec loop = function
    | Ident (ident, pos)
      :: L_paren
         :: String [StringStatic key]
            :: Comma
               :: Ident ("comment", _)
                  :: Colon :: String [StringStatic comment] :: R_paren :: rest
      when ident = routine_name ->
        let pos = {pos with pos_fname= filename} in
        let call = {key; comment; pos} in
        Queue.push call queue ; loop rest
    | String parts :: rest ->
        List.iter
          (function
            | StringStatic _ -> () | StringInterpolation ast -> loop ast )
          parts ;
        loop rest
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop ast

let rec walk dir f =
  let g filename =
    let path = Filename.concat dir filename in
    try if Sys.is_directory path then walk path f else f path with _ -> ()
  in
  Array.iter g (Sys.readdir dir)

let string_of_file filename =
  let ch = open_in_bin filename in
  let len = in_channel_length ch in
  let s = really_input_string ch len in
  close_in_noerr ch ; s

let genstrings ~routine_name dir =
  let call_queue = Queue.create () in
  let table = Hashtbl.create 8 in
  let visitor path =
    if Filename.extension path = ".swift" then
      let ast = Swift.parse_string @@ string_of_file path in
      collect_swift ~filename:path ~routine_name call_queue ast
    else
      let dotstrings = Filename.basename path in
      let lang_lproj = Filename.basename @@ Filename.dirname path in
      let lproj = Filename.extension lang_lproj in
      match (dotstrings, lproj) with
      | "Localizable.strings", ".lproj" ->
          let lang = Filename.chop_suffix lang_lproj lproj in
          let ast = Dotstrings.parse_string @@ string_of_file path in
          Hashtbl.replace table lang ast
      | _ -> ()
  in
  walk dir visitor ; (call_queue, table)
