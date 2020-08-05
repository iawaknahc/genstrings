type routine_call = { key : string; comment : string; pos : Lexing.position }

type new_value = Key | Comment | String of string

module StringSet = Set.Make (String)

exception ManyError of exn list

exception InconsistentComment of routine_call * routine_call

exception MissingDevLang

exception MoreThanOneDevLang of string list

let collect_swift ~filename ~routine_name queue ast =
  let open Swift in
  let rec loop = function
    | Ident (ident, pos)
      :: L_paren
         :: String [ StringStatic key ]
            :: Comma
               :: Ident ("comment", _)
                  :: Colon :: String [ StringStatic comment ] :: R_paren :: rest
      when ident = routine_name ->
        let pos = { pos with Lexing.pos_fname = filename } in
        let call = { key; comment; pos } in
        Queue.push call queue;
        loop rest
    | String parts :: rest ->
        List.iter
          (function
            | StringStatic _ -> () | StringInterpolation ast -> loop ast)
          parts;
        loop rest
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop ast

let collect_objc ~filename ~routine_name queue ast =
  let open Objc in
  let push pos key comment =
    let pos = { pos with Lexing.pos_fname = filename } in
    let call = { key; comment; pos } in
    Queue.push call queue
  in
  let rec loop = function
    | Ident (ident, pos)
      :: L_paren
         :: At :: String key :: Comma :: At :: String comment :: R_paren :: rest
      when ident = routine_name ->
        push pos key comment;
        loop rest
    | Ident (ident, pos)
      :: L_paren
         :: String key :: Comma :: At :: String comment :: R_paren :: rest
      when ident = routine_name ->
        push pos key comment;
        loop rest
    | Ident (ident, pos)
      :: L_paren
         :: At :: String key :: Comma :: String comment :: R_paren :: rest
      when ident = routine_name ->
        push pos key comment;
        loop rest
    | Ident (ident, pos)
      :: L_paren :: String key :: Comma :: String comment :: R_paren :: rest
      when ident = routine_name ->
        push pos key comment;
        loop rest
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop ast

let rec walk dir f =
  let g filename =
    let path = Filename.concat dir filename in
    if Sys.is_directory path then walk path f else f path
  in
  Array.iter g (Sys.readdir dir)

let string_of_file filename =
  let ch = open_in_bin filename in
  let len = in_channel_length ch in
  let s = really_input_string ch len in
  close_in_noerr ch;
  s

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
          Hashtbl.replace table call.key call;
          errors)
      [] queue
  in
  match errors with [] -> Ok table | _ -> Error errors

let remove_routine_calls seq key_set =
  Seq.map
    (fun (lang, ast, path) ->
      ( lang,
        List.filter
          (fun entry -> StringSet.mem entry.Dotstrings.key key_set)
          ast,
        path ))
    seq

let key_set_of_entries entries =
  entries
  |> List.map (fun entry -> entry.Dotstrings.key)
  |> List.to_seq |> StringSet.of_seq

let add_routine_calls entries call_table new_value =
  let key_set = key_set_of_entries entries in
  Hashtbl.fold
    (fun key value acc ->
      if StringSet.mem key key_set then acc
      else
        let new_value =
          match new_value with
          | Key -> value.key
          | Comment -> value.comment
          | String s -> s
        in
        let comment =
          if value.comment = "" then "No comment provided by engineer."
          else value.comment
        in
        let new_entry = { Dotstrings.key; value = new_value; comment } in
        new_entry :: acc)
    call_table entries

let add_entries from_entries to_entries =
  let to_key_set = key_set_of_entries to_entries in
  List.fold_left
    (fun acc entry ->
      if StringSet.mem entry.Dotstrings.key to_key_set then acc
      else entry :: acc)
    to_entries from_entries

let write_entries entries path =
  let ch = open_out_bin path in
  let ppf = Format.formatter_of_out_channel ch in
  Format.fprintf ppf "%a@." Dotstrings.pp entries;
  close_out_noerr ch

let discover routine_name dir =
  let call_queue = Queue.create () in
  let strings_queue = Queue.create () in
  let visitor path =
    match Filename.extension path with
    | ".swift" ->
        let ast = Swift.parse_string ~filename:path (string_of_file path) in
        collect_swift ~filename:path ~routine_name call_queue ast
    | ".m" | ".h" ->
        let ast = Objc.parse_string ~filename:path (string_of_file path) in
        collect_objc ~filename:path ~routine_name call_queue ast
    | _ -> (
        let dotstrings = Filename.basename path in
        let lang_lproj = Filename.basename @@ Filename.dirname path in
        let lproj = Filename.extension lang_lproj in
        match (dotstrings, lproj) with
        | "Localizable.strings", ".lproj" ->
            let lang = Filename.chop_suffix lang_lproj lproj in
            let ast =
              Dotstrings.parse_string ~filename:path (string_of_file path)
            in
            Queue.push (lang, ast, path) strings_queue
        | _ -> () )
  in
  walk dir visitor;
  (call_queue, strings_queue)

let genstrings ~routine_name ~devlang ~new_value dir =
  let call_queue, strings_queue = discover routine_name dir in
  let call_table =
    match verify_routine_calls call_queue with
    | Ok table -> table
    | Error errors -> raise @@ ManyError errors
  in
  let strings_list =
    remove_routine_calls
      (Queue.to_seq strings_queue)
      (StringSet.of_seq @@ Hashtbl.to_seq_keys call_table)
    |> List.of_seq
  in
  let devlang_list, other_list =
    List.partition (fun (lang, _, _) -> lang = devlang) strings_list
  in
  let _, devlang_entries, devlang_path =
    match devlang_list with
    | [ a ] -> a
    | [] -> raise MissingDevLang
    | _ ->
        raise
        @@ MoreThanOneDevLang (List.map (fun (_, _, path) -> path) devlang_list)
  in
  let devlang_entries =
    add_routine_calls devlang_entries call_table new_value
  in
  let other_list =
    List.map
      (fun (lang, entries, path) ->
        let entries = add_entries devlang_entries entries in
        (lang, entries, path))
      other_list
  in
  let all_list = (devlang, devlang_entries, devlang_path) :: other_list in
  let cmp entry1 entry2 =
    String.compare entry1.Dotstrings.key entry2.Dotstrings.key
  in
  let all_list =
    List.map
      (fun (lang, entries, path) -> (lang, List.sort cmp entries, path))
      all_list
  in
  List.iter (fun (_, entries, path) -> write_entries entries path) all_list
