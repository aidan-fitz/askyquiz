open Quiz
open Progress
open Yojson
open Yojson.Basic
open Yojson.Basic.Util
open QCheck

(** [mode] *)
type mode = Subjective | Test | Practice

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

(** [shuffle letters lst] returns association lists where the key is a 
    letter option and the values are an answer id in the shuffled list 
    of answer id and text pairs *)
let shuffle letters lst = 
  List.combine letters (QCheck.Gen.(generate1 (shuffle_l lst)))

(** [get_aid ltr mapping] is the answer id associated with the 
    letter option [ltr]
    Raises: Failure if letter is invalid*)
let rec get_aid ltr = function
  | [] -> failwith "Invalid answer option"  
  | (l, (id, _))::t when l = ltr -> id
  | _::t ->  get_aid ltr t 

(** [check_answer qid aid mode] updates progress and gives feedback according 
    to [mode]*)
let check_answer qid aid mode = ()


(** [ask qn is_odd quiz] displays [qn] to the screen and prompts for an answer
    among its choices in [quiz]. Answers are enumerated with A, B, C, D, E if 
    [is_odd is true], and with F, G, H, J, K otherwise. *)
let ask (qid, qtxt) is_odd quiz = 
  print_endline qtxt;

  (* print up to 5 answers; if there are fewer than 5, catch the exception *)
  try 
    let letters = (if is_odd then odd_letters else even_letters) in 
    let ans_pairs = (get_answers qid quiz) in
    let display_options = shuffle letters ans_pairs in
    List.iter
      (fun (letter, (id, ans)) -> print_endline (letter ^ ". " ^ ans))
      display_options 

  with Invalid_argument _ -> ();

    print_string "Answer: ";
    let input = read_line () in (*check answer*)
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

(** [ prompt_mode ()] returms the mode that the user wishes to play *)
let rec prompt_mode () = 
  print_string "Select (1) test or (2) practice mode > ";
  let choice = read_line () in 
  if choice = "1" then Test 
  else if choice = "2" then Practice 
  else print_endline "Sorry, try again"; prompt_mode ()


(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  print_string "Enter .quiz to load? > ";
  let fn = read_line () in 
  let quiz =  match load_quiz fn with
    | None -> print_endline "Sorry, try again"; main ()
    | Some q -> q (*if nonsub prompt for mode*)
  in
  print_string ""
let m
    if Quiz.subjective quiz then Subjective 
    else prompt_mode ()
(* Execute the game engine. *)
let () = main ()