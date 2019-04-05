open Cmdliner

type new_value = Key | Comment | Placeholder

let mapping = [("key", Key); ("comment", Comment); ("placeholder", Placeholder)]
let sprintf = Printf.sprintf

let devlang =
  let doc = "The development language" in
  let env = Arg.env_var "GENSTRINGS_DEV_LANG" ~doc in
  Arg.(value & opt string "en" & info ["dev-lang"] ~env ~docv:"DEV_LANG" ~doc)

let routine =
  let doc = "The routine name to scan for" in
  let env = Arg.env_var "GENSTRINGS_ROUTINE" ~doc in
  Arg.(
    value
    & opt string "NSLocalizedString"
    & info ["routine"] ~env ~docv:"ROUTINE" ~doc)

let new_value =
  let alts = Arg.doc_alts_enum mapping in
  let doc = sprintf "The new value to use; $(docv) must be %s" alts in
  let env = Arg.env_var "GENSTRINGS_NEW_VALUE" ~doc in
  Arg.(
    value
    & opt (enum mapping) Placeholder
    & info ["new-value"] ~env ~docv:"NEW_VALUE" ~doc)

let placeholder =
  let doc = "The placeholder for new value" in
  let env = Arg.env_var "GENSTRINGS_PLACEHOLDER" ~doc in
  Arg.(
    value
    & opt string "<YOUR COPY HERE>"
    & info ["placeholder"] ~env ~docv:"PLACEHOLDER" ~doc)

let dir =
  let doc = "The directory to scan" in
  let env = Arg.env_var "GENSTRINGS_DIR" ~doc in
  Arg.(
    value & pos 0 dir Filename.current_dir_name & info [] ~env ~docv:"DIR" ~doc)

let write_error buf pos msg =
  let filename = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  Buffer.add_string buf (sprintf "%s:%d:%d: %s\n" filename line col msg)

let print_exn ~devlang exn =
  let buf = Buffer.create 17 in
  let rec loop = function
    | Dotstrings.ParseError (msg, s, _) -> write_error buf s msg
    | Swift.ParseError (msg, s, _) -> write_error buf s msg
    | Genstrings.MissingDevLang ->
        let msg =
          sprintf "expected %s.lproj/Localizable.strings to exist\n" devlang
        in
        Buffer.add_string buf msg
    | Genstrings.InconsistentComment (_, curr) ->
        let msg =
          sprintf "routine call `%s' has different comment\n"
            curr.Genstrings.key
        in
        write_error buf curr.Genstrings.pos msg
    | Genstrings.ManyError exns -> List.iter (fun exn -> loop exn) exns
    | _ -> Buffer.add_string buf "unknown error\n"
  in
  loop exn ;
  prerr_string @@ Buffer.contents buf

let genstrings devlang routine new_value placeholder dir =
  let new_value =
    match new_value with
    | Key -> Genstrings.Key
    | Comment -> Genstrings.Comment
    | Placeholder -> Genstrings.String placeholder
  in
  try Genstrings.genstrings ~routine_name:routine ~devlang ~new_value dir
  with e -> print_exn ~devlang e ; exit 1

let cmd =
  let doc = "generate string table from source code" in
  ( Term.(const genstrings $ devlang $ routine $ new_value $ placeholder $ dir)
  , Term.info "genstrings" ~doc ~exits:Term.default_exits )

let () = Term.(exit @@ eval ~catch:false cmd)
