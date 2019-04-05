type routine_call = {key: string; comment: string; pos: Lexing.position}

let collect_swift ~filename ~routine_name ast =
  let queue = Queue.create () in
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
  loop ast ;
  List.of_seq @@ Queue.to_seq queue

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

let collect_swift_file filename =
  let s = string_of_file filename in
  let ast = Swift.parse_string s in
  let calls = collect_swift ~filename ~routine_name:"NSLocalizedString" ast in
  calls
