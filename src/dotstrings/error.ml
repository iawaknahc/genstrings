(* This file was auto-generated based on "plist/parse/error.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message s =
  match s with
  | 0 ->
      "expecting a comment\n"
  | 1 ->
      "expecting a key\n"
  | 4 ->
      "expecting a `='\n"
  | 5 ->
      "expecting a value\n"
  | 6 ->
      "expecting a `;'\n"
  | 11 ->
      "expecting a comment.\n"
  | _ ->
      raise Not_found
