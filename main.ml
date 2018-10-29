open Quiz
open Progress
open Yojson
open Yojson.Basic
open Yojson.Basic.Util
open QCheck
open ANSITerminal

(** The current testing mode. *)
type mode = Subjective | Test | Practice

exception Interrupt

(** [load_quiz f] is the [Quiz.t] created from the quiz JSON in file [f]. 
    If the JSON does not represent a valid quiz, it reprompts for a file. *)
let rec load_quiz () = 
  let f = read_line () in
  let quiz = 
    try Some (parse_json f)
    with 
    | Sys_error _  -> print_string [yellow] "File not found. "; None
    | Json_error _ -> print_string [yellow]
                        "File does not contain valid JSON. "; None
    | Type_error _ -> print_string [yellow] 
                        "JSON does not represent quiz. "; None
  in match quiz with 
  | Some q -> q
  | None -> 
    print_string [yellow] "Try again:\n";
    print_string [] "> ";
    load_quiz ()

(* let odd_letters  = ["A"; "B"; "C"; "D"; "E"]
   let even_letters = ["F"; "G"; "H"; "J"; "K"] *)

(** [next l] is the letter after [l] in the alphabet. *)
let next l = Char.(chr ((code l - code 'A' + 1) mod 26 + code 'A'))

(** [make_letters n is_odd] is a list of [n] letters starting from 'A' if 
    [is_odd] and starting from 'F' otherwise. The sequence always skips 'I'. *)
let make_letters n is_odd =
  let rec go n l acc =
    if n = 0 then acc else
    if (next l) = 'I' 
    then go (n-1) (l |> next |> next) ((String.make 1 l)::acc)
    else go (n-1) (l |> next) ((String.make 1 l)::acc)
  in List.rev (if is_odd then go n 'A' [] else go n 'F' [])

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

(** [imm_feedback correct_aid correct options] provides immediate feedback for 
    an answer to a question under practice mode *)
let imm_feedback correct_aid correct options =
  if correct then print_string [green] "You are correct!\n"
  else let option = 
         List.find (fun (ltr, (id, text)) -> id = correct_aid) options in
    let () = print_string [red] ("Incorrect. The correct answer is " ^ 
                                 (fst option) ^ ". " ^ (snd (snd option))) in 
    print_newline ()

(** [requeue qid mstry correct] is whether the question needs to be 
    requeued. It also updates the mastery level of a question based on the 
    answer given. *)
let requeue qid mstry correct =
  let m = List.assoc qid mstry in
  if correct then let () = m := !m + 1 in (not (m = ref 3))
  else let () = (if (not (m = ref 0)) then (m := !m - 1)) in true

(** [check_answer qid aid mode] updates progress and gives feedback according 
    to [mode]*)
let check_answer qid aid mode options prog quiz = 
  let rq =
    if mode = Practice then 
      let correct_aid = qid |> answers quiz |> correct_ans in
      let correct = aid = correct_aid in
      imm_feedback correct_aid correct options;
      requeue qid (mastery prog) correct
    else false
  in
  update_scores qid aid quiz prog;
  pop_and_requeue rq prog

(** [prompt_answer ltrs] retrieves the user answer from the terminal and 
    ensures that it is a valid answer option, prompting again if not*)
let rec prompt_answer ltrs = 
  print_string [] "Answer: ";
  let input = String.uppercase_ascii (read_line ()) in
  if List.mem input ltrs then input
  else let () = print_string [yellow] "Invalid answer option, try again.\n" in
    prompt_answer ltrs

(** [ask qn is_odd mode quiz prog] displays [qn] to the screen and 
    prompts for an answer among its choices in [quiz]. Answers are enumerated 
    with A, B, C, D, E if [is_odd is true], and with F, G, H, J, K otherwise. *)
let rec ask q is_odd mode quiz prog = 
  match q with 
  | None -> prog
  | Some q -> 
    let qid = q in
    let qtxt = (get_txt_from_id q quiz) in
    print_newline ();
    print_endline qtxt;

    let ans_pairs = (get_answers qid quiz) in
    let ltrs = make_letters (List.length ans_pairs) is_odd in 
    let options = shuffle ltrs ans_pairs in
    (* print up to 5 answers; if there are fewer than 5, catch the exception *)
    List.iter (fun (ltr, (id, ans)) -> 
        print_endline (ltr ^ ". " ^ ans)) options;

    try 
      let input = prompt_answer ltrs in
      let aid = get_aid input options in
      let prog' = check_answer qid aid mode options prog quiz in
      let q' = next_question prog' in
      ask q' (not is_odd) mode quiz prog'
    with Interrupt -> Progress.save_progress (filename quiz) prog; prog

(** [ prompt_mode ()] returms the mode that the user wishes to play *)
let rec prompt_mode () = 
  print_string [] "Select (1) test or (2) practice mode > ";
  let choice = read_line () in 
  if choice = "1" then Test 
  else if choice = "2" then Practice 
  else let () = print_string [yellow] "Sorry, try again\n" in 
    prompt_mode ()

let handle_sigint () =
  Sys.(set_signal sigint (Signal_handle (fun _ -> raise Interrupt)))

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  handle_sigint ();
  print_string [] "Enter .quiz to load? > ";
  let quiz = load_quiz () in
  let quiz_length = List.length (get_questions quiz) in
  let mode = if (subjective quiz) then Subjective else prompt_mode () in
  let prog = init_progress quiz in
  let q = next_question prog in
  let end_prog = ask q true mode quiz prog in
  if next_question prog = None then
    match mode with
    | Subjective -> let () = print_string [Bold; cyan] 
                        ("\nYou have completed the quiz. You got: " ^ 
                         (best_category end_prog)) in print_newline ()
    | Test -> print_string [Bold; cyan] 
                "\nYou have completed the quiz. Your score is ";
      ANSITerminal.printf [Bold; cyan] "%.2f" 
        ((float_of_int ((best_score end_prog) * 10000 / quiz_length)) /. 100.0);
      print_string [Bold; cyan] "%.\n";
    | Practice -> print_string [Bold; cyan] "\nCongratulations, you have \
                                             mastered all questions in this \
                                             quiz!\n"
  else print_newline (); print_string [cyan] "Your progress is saved!"
(* Execute the game engine. *)
let () = main ()