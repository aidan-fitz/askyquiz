open Quiz
open Progress
open Yojson
open Yojson.Basic
open Yojson.Basic.Util

(** [load_quiz f] is the [Quiz.t] created from the quiz JSON 
    with filename [f]. *)
let load_quiz f = 
  try 
    let j = from_file f in
    Some (parse_json j)
  with 
  | Sys_error _ -> print_endline "File not found"; None
  | Json_error _ -> print_endline "File does not contain valid JSON"; None
  | Type_error _ -> print_endline "JSON does not represent adventure"; None

let odd_letters  = ["A"; "B"; "C"; "D"; "E"]
let even_letters = ["F"; "G"; "H"; "J"; "K"]

(** [ask qn is_odd quiz] displays [qn] to the screen and prompts for an answer
    among its choices in [quiz]. Answers are enumerated with A, B, C, D, E if 
    [is_odd is true], and with F, G, H, J, K otherwise. *)
let ask (qid, qtxt) is_odd quiz = 
  print_endline qtxt;

  (* print up to 5 answers; if there are fewer than 5, catch the exception *)
  try List.iter2
      (fun (_, ans) letter -> print_endline (letter ^ ". " ^ ans))
      (get_answers qid quiz)
      (if is_odd then odd_letters else even_letters)
  with Invalid_argument _ -> ();

    print_string "Answer: ";
    let input = read_line () in
    print_endline ("You answered " ^ input ^ "\n")

(** [run_quiz q] plays through quiz [q] with the user. *)
let run_quiz quiz =
  let is_odd = ref true in
  let rec run = function
    | [] -> ()
    | q::qs ->
        ask q !is_odd quiz;
        is_odd := not !is_odd;
        run qs
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