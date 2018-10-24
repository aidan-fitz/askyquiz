open Quiz
open Progress
open Yojson
open Yojson.Basic
open Yojson.Basic.Util

(** [load_quiz f] is the [Quiz] object created from the quiz
    JSON with filename [f]. *)
let load_quiz f = 
    try 
        let j = from_file f in
        Some (parse_json j)
    with 
    | Sys_error _ -> print_endline "File not found"; None
    | Json_error _ -> print_endline "File does not contain valid JSON"; None
    | Type_error _ -> print_endline "JSON does not represent adventure"; None

let answer_letters_odd  = ["A"; "B"; "C"; "D"; "E"]
let answer_letters_even = ["F"; "G"; "H"; "J"; "K"]

(** [ask qn quiz] displays [qn] to the screen and prompts for an answer
    among its choices in [quiz]. *)
let ask (qid, qs) quiz = 
    print_endline qs;
    (* print up to 5 answer choices; if there are fewer than 5, catch the exception *)
    try List.iter2
        (fun (_, ans) letter -> print_endline (letter ^ ". " ^ ans))
        (get_answers qid quiz)
        answer_letters_odd
    with Invalid_argument _ -> ();

    print_string "Answer: ";
    let input = read_line () in
    print_endline ("You answered " ^ input ^ "\n")

(** [run_quiz q] plays through quiz [q] with the user. *)
let run_quiz quiz =
    let rec run = function
    | [] -> ()
    | q::qs -> ask q quiz; run qs
    in run (get_questions quiz)

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
    print_string "Enter .quiz to load? > ";
    let fn = read_line () in
    match load_quiz fn with
    | None -> print_endline "Sorry, try again"; main ()
    | Some q -> run_quiz q

(* Execute the game engine. *)
let () = main ()