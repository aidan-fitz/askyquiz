open Quiz
open Progress
open Yojson
open Yojson.Basic
open Yojson.Basic.Util
open QCheck

(** [mode] *)
type mode = Subjective | Test | Practice

(** [load_quiz f] is the [Quiz.t] created from the quiz JSON with filename [f]. 
    If [f] is not a valid quiz, it prompts for another input. *)
let rec load_quiz () = 
  let f = read_line () in
  let quiz = 
    try 
      let j = from_file f in
      Some (parse_json j)
    with 
    | Sys_error _ -> print_endline "File not found"; None
    | Json_error _ -> print_endline "File does not contain valid JSON"; None
    | Type_error _ -> print_endline "JSON does not represent adventure"; None
  in match quiz with 
  | Some q -> q
  | None -> print_endline "Sorry, try again"; load_quiz ()


let odd_letters  = ["A"; "B"; "C"; "D"; "E"]
let even_letters = ["F"; "G"; "H"; "J"; "K"]

(** [shuffle letters lst] returns association lists where the key is a 
    letter option and the values are an answer id in the shuffled list 
    of answer id and text pairs *)
let shuffle ltrs lst = 
  List.combine ltrs (QCheck.Gen.(generate1 (shuffle_l lst)))

(** [get_aid ltr mapping] is the answer id associated with the 
    letter option [ltr]
    Raises: Failure if letter is invalid*)
let rec get_aid ltr = function
  | [] -> failwith "Invalid answer option"  
  | (l, (id, _))::t when l = ltr -> id
  | _::t ->  get_aid ltr t 

(** [check_answer qid aid mode] updates progress and gives feedback according 
    to [mode]*)
let check_answer qid aid mode mapping prog quiz = 
  if mode = Practice then 
    let correct_aid = qid |> get_answers |> correct_ans in
    if aid = correct_aid then print_endline "You are correct!"
    else let option = 
           List.find (fun (ltr, (id, text)) -> id = correct_aid) mapping in
      print_endline ("Incorrect. The correct answer is " ^ (fst option) ^ ". " 
                     ^ (snd (snd option)))
  else ();
  update_progress qid aid prog quiz

(** [ask qn is_odd quiz] displays [qn] to the screen and prompts for an answer
    among its choices in [quiz]. Answers are enumerated with A, B, C, D, E if 
    [is_odd is true], and with F, G, H, J, K otherwise. *)
let rec ask q is_odd mode quiz prog = 
  match q with 
  | None -> prog
  | Some q -> let (qid, qtxt) = q in
    print_endline qtxt;

    let ltrs = (if is_odd then odd_letters else even_letters) in 
    let ans_pairs = (get_answers qid quiz) in
    let options = shuffle ltrs ans_pairs in
    (* print up to 5 answers; if there are fewer than 5, catch the exception *)
    let () = try 
        List.iter
          (fun (ltr, (id, ans)) -> print_endline (ltr ^ ". " ^ ans)) options
      with Invalid_argument _ -> () in

    let () = print_string "Answer: " in
    let input = read_line () in (*check answer*)
    let aid = get_aid input options in
    let prog' = check_answer qid aid mode options prog quiz in
    let q' = next_question prog' in
    ask q' (not is_odd) mode quiz prog'

(** [ prompt_mode ()] returms the mode that the user wishes to play *)
let rec prompt_mode () = 
  print_string "Select (1) test or (2) practice mode > ";
  let choice = read_line () in 
  if choice = "1" then Test 
  else if choice = "2" then Practice 
  else let () = print_endline "Sorry, try again" in prompt_mode ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "Enter .quiz to load? > ";
  let quiz = load_quiz () in
  let quiz_length = List.length (get_questions quiz) in
  let m = if (subjective quiz) then Subjective else prompt_mode () in
  let prog = init_progress quiz in
  let q = next_question prog in
  let end_prog = ask q true m quiz prog in
  match m with
  | Subjective -> print_endline ("You have completed the quiz. You got: " ^ 
                                 (best_category end_prog))
  | Test -> print_string "You have completed the quiz. Your score is ";
    Printf.printf "%.2f" (best_score end_prog /. quiz_length);
    print_endline "."
  | Practice -> print_endline "Congratulations, you have mastered all \
                               questions in this quiz!"

(* Execute the game engine. *)
let () = main ()