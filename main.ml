open Quiz
open Progress
open Yojson
open Yojson.Basic
open Yojson.Basic.Util
open QCheck
open ANSITerminal
open Str
open Builder

(** Custom exception to help catch interrupt signals. *)
exception Interrupt

(** [load_quiz ()] is the [Quiz.t] created from the quiz JSON in file [f]. 
    If the JSON does not represent a valid quiz, it reprompts for a file. *)
let load_quiz () = 
  print_string [] "Enter .quiz to load: ";
  let rec load () =
    let f = "." ^ Filename.dir_sep ^ "quizzes" ^ Filename.dir_sep ^ 
            read_line () ^ ".quiz" in
    let quiz = 
      try Ok (parse_json f)
      with 
      | Sys_error _  -> Error "File not found."
      | Json_error _ -> Error "File has invalid JSON."
      | Type_error _ -> Error "JSON doesn't represent quiz."
    in match quiz with 
    | Ok q -> q
    | Error msg -> 
      print_string [yellow] (msg ^ " Try again:\n");
      print_string [] "> ";
      load ()
  in load ()

(** [next l] is the letter after [l] in the alphabet. *)
let next l = 
  let incr = if l = 'I' then 2 else 1 in
  Char.(chr ((code l - code 'A' + incr) mod 26 + code 'A'))

(** [make_letters n is_odd] is a list of [n] letters starting from 'A' if 
    [is_odd] and starting from 'F' otherwise. The sequence always skips 'I'. *)
let make_letters n is_odd =
  let rec go n l acc =
    if n = 0 then acc else
      go (n-1) (l |> next) ((String.make 1 l)::acc)
  in List.rev (if is_odd then go n 'A' [] else go n 'F' [])

(** [shuffle letters answers] is an association list resulting from pairing
    [letters] to a shuffled permutation of [answers]. *)
let shuffle ltrs answers = 
  List.combine ltrs (QCheck.Gen.(generate1 (shuffle_l answers)))

(** [get_aid ltr mapping] is the answer id associated with the 
    letter option [ltr] in [mapping].
    Raises: Failure if letter is invalid. *)
let rec get_aid ltr = function
  | [] -> failwith "Invalid answer option"  
  | (l, (id, _))::t when l = ltr -> id
  | _::t ->  get_aid ltr t 

(** [imm_feedback correct_aid correct options] outputs immediate feedback for 
    an answer to a question under practice mode. *)
let imm_feedback correct_aid correct options =
  if correct then print_string [green] "You are correct!\n"
  else
    let option = List.find (fun (ltr, (id, text)) -> id = correct_aid) options
    in print_string [red]
      ("Incorrect. The correct answer is " ^
       (fst option) ^ ". " ^ (snd (snd option)));
    print_newline ()

(** [requeue qid mstry correct] determines whether the question [qid] should be 
    requeued. It also updates the question's mastery level based on [correct].*)
let requeue qid mstry correct =
  let m = List.assoc qid mstry in
  if correct then let () = m := !m + 1 in (not (m = ref 3))
  else ((if (not (m = ref 0)) then (m := !m - 1)); true)

(** [check_answer qid aid ans_choices prog quiz] updates [prog] and gives 
    feedback according to [quiz_mode prog]. *)
let check_answer qid aid ans_choices prog quiz = 
  let rq =
    if quiz_mode prog = Practice then 
      let correct_aid = qid |> answers quiz |> correct_ans in
      let is_correct = aid = correct_aid in
      imm_feedback correct_aid is_correct ans_choices;
      requeue qid (mastery prog) is_correct
    else false
  in
  update_scores qid aid quiz prog;
  pop_and_requeue rq prog

(** [prompt_answer ltrs] retrieves the user's answer as input and checks that 
    it exists in [ltrs], prompting again if not.
    Requires: [ltrs] only contains uppercase letters. *)
let rec prompt_answer ltrs = 
  print_string [] "Answer: ";
  let input = String.uppercase_ascii (read_line ()) in
  if List.mem input ltrs then input
  else 
    (print_string [yellow] "Invalid answer option, try again.\n";
     prompt_answer ltrs)

(** [ask qn is_odd quiz prog] displays [qn] to the screen and 
    prompts for an answer among its choices in [quiz]. It lists answers with 
    A, B, C, D, E if [is_odd] is [true], and with F, G, H, J, K otherwise. *)
let rec ask q is_odd quiz prog = 
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
      let prog' = check_answer qid aid options prog quiz in
      let q' = next_question prog' in
      ask q' (not is_odd) quiz prog'
    with
    | Interrupt | End_of_file -> Progress.save_progress prog

(** [prompt_mode ()] is the quiz mode the user selects to play in. *)
let rec prompt_mode () = 
  print_string [] "Select (1) test or (2) practice mode > ";
  match read_line () with
  | "1" -> Test
  | "2" -> Practice
  | _ -> print_string [yellow] "Sorry, try again\n"; prompt_mode ()

(** [handle_sigint ()] sets up a handler for SIGINT *)
let handle_sigint () =
  Sys.(set_signal sigint (Signal_handle (fun _ -> raise Interrupt)))

(** [edit ()] opens the .quiz file the user inputs in vim. If the user does not
    input a valid quiz, it reprompts for another file. *)
let edit () =
  print_string [] "Enter .quiz to edit > ";
  let rec open_file () = 
    let file = 
      ("." ^ Filename.dir_sep ^ "quizzes" ^ Filename.dir_sep ^ read_line ()) in
    if (Sys.file_exists file) && (Str.string_match (regexp ".*.quiz") file 0) 
    then ignore (Unix.system ("vim " ^ file))
    else
      (print_string [yellow] "File is not an existing quiz file. Try again:\n";
       print_string [] "> ";
       open_file ())
  in open_file ()

let print_quizzes () =
  print_endline "Available quizzes:";
  let dir = Unix.opendir ("." ^ Filename.dir_sep ^ "quizzes") in
  let rec quizzes () = 
    try let f = Unix.readdir dir in 
      if (Str.string_match (regexp ".*.quiz") f 0) then print_endline f else ();
      quizzes ()
    with End_of_file -> Unix.closedir dir
  in quizzes ()
(** [take_quiz ()] runs the quiz the user enters. If the user does not input a 
    valid quiz, it reprompts for another file. *)
let take_quiz () =
  print_quizzes ();
  let quiz = load_quiz () in
  print_string [magenta] (title quiz);
  print_newline ();
  print_string [magenta] (desc quiz);
  print_newline ();
  let quiz_length = List.length (get_questions quiz) in
  let prog = get_progress quiz begin
      fun () -> if (subjective quiz) then Subjective else prompt_mode ()
    end in
  let q = next_question prog in
  let end_prog = ask q true quiz prog in
  if next_question end_prog = None then
    ((try (Sys.remove (Progress.filename prog))
      with Sys_error _ -> ());
     match quiz_mode prog with
     | Subjective ->
       print_string [Bold; cyan] 
         ("\nYou have completed the quiz. You got: " ^ 
          (best_category end_prog)); print_newline ()
     | Test ->
       print_string [Bold; cyan] 
         "\nYou have completed the quiz. Your score is ";
       ANSITerminal.printf [Bold; cyan] "%.2f" 
         ((float_of_int ((best_score end_prog) * 10000 / quiz_length)) /. 100.0);
       print_string [Bold; cyan] "%.\n";
     | Practice ->
       print_string [Bold; cyan]
         "\nCongratulations, you have mastered all questions in this quiz!\n")
  else (print_newline (); print_string [cyan] "Your progress is saved!\n")

let rec menu () = 
  print_string [Bold]
    "\nMenu: \n\
     1. Create a new quiz \n\
     2. Edit an existing quiz \n\
     3. Take a quiz";
  print_newline ();
  print_string [] "Select (1) create, (2) edit, or (3) take quiz mode > ";
  match read_line () with
  | "1" -> builder ()
  | "2" -> edit ()
  | "3" -> take_quiz ()
  | _ -> print_string [yellow] "Invalid mode; try again > "; menu ()


let welcome_message () = 
  resize 100 30;
  print_string [red; on_black] "

  "

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  handle_sigint ();
  welcome_message ();
  menu ()

(* Execute the game engine. *)
let () = main ()