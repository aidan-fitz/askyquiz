open Quiz
open Progress
open Yojson
open Yojson.Basic
open Yojson.Basic.Util
open QCheck
open ANSITerminal
open Str
open Builder
open Validation

(** Custom exception to help catch interrupt signals. *)
exception Interrupt

(************ MISC. HELPERS *************)
(* Alias for directory separator. *)
let slash = Filename.dir_sep

(** [next l] is the letter after [l] in the alphabet, but skips 'I'. *)
let next l = 
  let incr = if l = 'H' then 2 else 1 in
  Char.(chr ((code l - code 'A' + incr) mod 26 + code 'A'))

(** [make_letters n is_odd] is a list of [n] letters starting from 'A' if 
    [is_odd] and starting from 'F' otherwise. The sequence always skips 'I'. *)
let make_letters n is_odd =
  let rec go n l acc =
    if n = 0 then acc
    else go (n-1) (l |> next) ((String.make 1 l)::acc)
  in go n (if is_odd then 'A' else 'F') [] |> List.rev

(** [shuffle letters answers] is an association list resulting from pairing
    [letters] to a shuffled permutation of [answers]. *)
let shuffle ltrs answers = 
  List.combine ltrs (QCheck.Gen.(generate1 (shuffle_l answers)))

(**************** READING INPUT  ****************)
(** [load_quiz ()] is the [Quiz.t] created from the quiz JSON in file [f]. 
    If the JSON does not represent a valid quiz, it reprompts for a file. *)
let load_quiz () = 
  print_string [] "Enter quiz name to load:\n> ";
  let rec load () =
    let f = "." ^ slash ^ "quizzes" ^ slash ^ read_line () ^ ".quiz" in
    let quiz = parse_json f
    in match quiz with 
    | Ok q -> q
    | Error msg -> 
      print_string [yellow] (msg ^ " Try again:\n");
      print_string [] "> ";
      load ()
  in load ()

(** [prompt_mode ()] is the quiz mode the user selects to play in. *)
let rec prompt_mode () = 
  print_string [] "Select (1) test or (2) practice mode\n> ";
  let rec read_input () =
    match read_line () with
    | "1" -> Test
    | "2" -> Practice
    | _ -> (print_string [yellow] "Invalid mode. Try again\n";
            print_string [] "> ";
            read_input ())
  in read_input ()

(**************** PRINTING INFO ****************)
(** [imm_feedback correct_aid correct options] outputs immediate feedback for 
    an answer to a question under practice mode. *)
let imm_feedback correct_aid correct options =
  if correct then print_string [green] "You are correct!\n"
  else
    let option = List.find (fun (_, (id, _)) -> id = correct_aid) options
    in printf [red]
      "Incorrect. The correct answer is %s. %s\n" (fst option) (snd (snd option))

(** [print_by_type ()] displays subjective and non-subjective quizzes by 
    type. *)
let print_by_type () = 
  ignore(Unix.system "cd ./quizzes && grep -l \'\"subjective\": true\' *.quiz \
                      > sub.log && cd ..");
  ignore(Unix.system "cd ./quizzes && grep -l \'\"subjective\": false\' *.quiz \
                      > not.log && cd ..");
  let rec print_q f = 
    try 
      let line = input_line f in  
      let n = Str.search_forward (regexp ".quiz") line 0 in
      let line' = Str.string_before line n in
      print_string [magenta] (line' ^ "\t");
      print_q f
    with End_of_file -> close_in f
  in
  let s = open_in ("." ^ slash ^ "quizzes/sub.log") in
  let n = open_in ("." ^ slash ^ "quizzes/not.log") in
  print_string [Bold] "Subjective quizzes: \n";
  print_q s; print_newline ();
  print_string [Bold] "Non-subjective quizzes: \n";
  print_q n; print_newline ()

(**************** STATE LOGIC ****************)
(** [final_score q p] returns a percentage as the user's final score for quiz
    [q] after progress [p]. *)
let final_score q p =
  let n = List.length (get_questions q) in
  (float_of_int (best_score p * 10000 / n) /. 100.0)

(** [requeue qid mstry correct] determines whether the question [qid] should be 
    requeued. It also updates the question's mastery level based on [correct].*)
let requeue qid mstry correct =
  let m = List.assoc qid mstry in
  if correct then (m := !m + 1; !m <> 3)
  else ((if !m <> 0 then (m := !m - 1)); true)

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

(**************** MAIN MENU METHODS ****************)
(** [prompt_answer options] prompts the user for input and then checks it 
    against the [options] data structure using [Validation.user_answer]. The
    function re-prompts if the user's input is invalid. *)
let rec prompt_answer options =
  print_string [] "Answer: ";
  let input = read_line () in
  match user_answer input options with
  | Ok id -> id
  | Error msg ->
    begin
      print_string [yellow] "Invalid response; try again.\n";
      prompt_answer options
    end

(** [ask qn is_odd quiz prog] displays [qn] to the screen and 
    prompts for an answer among its choices in [quiz]. It lists answers with 
    A, B, C, D, E if [is_odd] is [true], and with F, G, H, J, K otherwise. *)
let rec ask q is_odd quiz prog = 
  match q with 
  | None -> prog
  | Some q -> 
    let qid = q in
    let qtxt = (get_txt_from_id q quiz) in
    print_string [Bold] ("\n" ^ qtxt ^ "\n");

    let ans_pairs = (get_answers qid quiz) in
    let ltrs = make_letters (List.length ans_pairs) is_odd in 
    let options = shuffle ltrs ans_pairs in
    (* print up to 5 answers; if there are fewer than 5, catch the exception *)
    List.iter (fun (l, (id, ans)) -> print_endline (l ^ ". " ^ ans)) options;

    try 
      let aid = prompt_answer options in
      let prog' = check_answer qid aid options prog quiz in
      let q' = next_question prog' in
      ask q' (not is_odd) quiz prog'
    with
    | Interrupt | End_of_file -> Progress.save_progress prog

(** [handle_sigint ()] sets up a handler for SIGINT *)
let handle_sigint () =
  Sys.(set_signal sigint (Signal_handle (fun _ -> raise Interrupt)))

(** [edit ()] opens the .quiz file the user inputs in vim. If the user does not
    input a valid quiz, it reprompts for another file. *)
let edit () =
  print_newline ();
  print_by_type ();
  print_string [] "Enter quiz name to edit:\n> ";
  let rec open_file () = 
    let file = 
      ("." ^ slash ^ "quizzes" ^ slash ^ read_line () ^ ".quiz") in
    if (Sys.file_exists file) && (Str.string_match (regexp ".*.quiz") file 0) 
    then (ignore (Unix.system ("vim " ^ file)))
    else
      (print_string [yellow] "File is not an existing quiz file. Try again:\n";
       print_string [] "> ";
       open_file ())
  in open_file ()

(** [take_quiz ()] runs the quiz the user enters. If the user does not input a 
    valid quiz, it reprompts for another file. *)
let take_quiz () =
  print_newline ();
  print_by_type ();
  let quiz = load_quiz () in
  print_newline ();
  printf [Bold; magenta] "%s\n" (title quiz);
  printf [magenta] "%s\n" (desc quiz);
  let prog = get_progress quiz begin
      fun () -> if (subjective quiz) then Subjective else prompt_mode ()
    end in
  let q = next_question prog in
  let prog' = ask q true quiz prog in
  if next_question prog' = None then
    (try Sys.remove (Progress.filename prog) with Sys_error _ -> ();
       match quiz_mode prog with
       | Subjective ->
         printf [Bold; cyan] 
           "\nYou have completed the quiz. You got: %s\n" (best_category prog')
       | Test ->
         printf [Bold; cyan]
           "\nYou have completed the quiz. Your score is %.2f%%\n"
           (final_score quiz prog');
       | Practice ->
         print_string [Bold; cyan]
           "\nCongratulations, you have mastered all questions in this quiz!\n")
  else print_string [cyan] "\nYour progress is saved!\n"

let rec welcome_menu () = 
  print_string [Bold]
    "\nMenu: \n\
     1. Create a new quiz \n\
     2. Edit an existing quiz \n\
     3. Take a quiz\n";
  print_string [] "Select (1) create, (2) edit, or (3) take quiz mode\n> ";
  let rec read_input () = 
    match read_line () with
    | "1" -> builder ()
    | "2" -> edit ()
    | "3" -> take_quiz ()
    | _ -> (print_string [yellow] "Invalid mode. Try again:\n";
            print_string [] "> ";
            read_input ())
  in read_input ()

let welcome_message () = 
  resize 100 30;
  print_string [red; on_black] "

 _ _ _     _                      _____     
| | | |___| |___ ___ _____ ___   |_   ____  
| | | | -_| |  _| . |     | -_|    | || . | 
|_____|___|_|___|___|_|_|_|___|    |_||___| 


 _____ _____ _____ __ __    _____     _     
|  _  |   __|  |  |  |  |  |     |_ _|_|___ 
|     |__   |    -|_   _|  |  |  | | | |- _|
|__|__|_____|__|__| |_|    |__  _|___|_|___|
                              |__|          
"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  handle_sigint ();
  welcome_message ();
  welcome_menu ()

(* Execute the game engine. *)
let () = main ()