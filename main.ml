open Quiz
open Progress
open Yojson
open Yojson.Basic
open Yojson.Basic.Util

(** [load_quiz f] is the [Quiz] object created from the quiz
    JSON with filename [f]*)
let load_quiz f = 
  try 
    let j = from_file f in
    Some (parse_json j)
  with 
  | Sys_error _ -> print_endline "File not found"; None
  | Json_error _ -> print_endline "File does not contain valid JSON"; None
  | Type_error _ -> print_endline "JSON does not represent adventure"; None

(** [display qxn ans] is the string to display when prompting the user
    for an answer*)
let display qxn ans = 
  failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () = ()

(* Execute the game engine. *)
let () = main ()