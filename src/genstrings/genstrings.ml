type routine_call = {key: string; comment: string; pos: Lexing.position}
type new_value = Key | Comment | String of string

module StringSet = Set.Make (String)

exception ManyError of exn list
exception InconsistentComment of routine_call * routine_call

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

let verify_routine_calls queue =
  let len = Queue.length queue in
  let table = Hashtbl.create len in
  let errors =
    Queue.fold
      (fun errors call ->
        try
          let prev = Hashtbl.find table call.key in
          if prev.comment <> call.comment then
            InconsistentComment (prev, call) :: errors
          else errors
        with Not_found ->
          Hashtbl.replace table call.key call ;
          errors )
      [] queue
  in
  match errors with [] -> Ok (Hashtbl.to_seq_keys table) | _ -> Error errors

let remove_routine_calls seq key_set =
  Seq.map
    (fun (lang, ast, path) ->
      ( lang
      , List.filter
          (fun entry -> StringSet.mem entry.Dotstrings.key key_set)
          ast
      , path ) )
    seq

let genstrings ~routine_name ~devlang ~new_value dir =
  (* Discover routine calls and Localizable.strings *)
  let call_queue = Queue.create () in
  let strings_queue = Queue.create () in
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
          Queue.push (lang, ast, path) strings_queue
      | _ -> ()
  in
  walk dir visitor ;
  (* Verify the same routine call has the same comment *)
  let key_set =
    match verify_routine_calls call_queue with
    | Ok key_seq -> StringSet.of_seq key_seq
    | Error errors -> raise @@ ManyError errors
  in
  let strings_seq =
    remove_routine_calls (Queue.to_seq strings_queue) key_set
  in
  ()
