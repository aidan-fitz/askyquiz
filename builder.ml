(** Allows JSON-formatted .quiz files to be generated by user *)

open Yojson.Basic.Util
open Quiz
open ANSITerminal


(** [strings_to_json ls] is a JSON representation of [ls]. *)
let strings_to_json (ls: string list) = 
  `List (List.map (fun s -> `String s) ls)

(** [build_list f acc cats n] builds a [n]-length list [acc] that uses function
    [f] and prefix [pref] to help create the ids for the elements of [acc]  *)
let rec build_list f acc cats pref n =
  if n = 0 then `List acc else
    let acc' = (f (pref^"id"^ (string_of_int n)) cats)::acc in
    build_list f acc' cats pref (n-1)

(** [ans_list id cats] is a JSON representation of answers with [cats] 
    scoring categories*)
let ans_list id cats = 
  `Assoc ([("id", `String id); 
           ("text", `String ("Replace this text with your answer option. "^
                             "Replace values of categories below with 1 if "^
                             "this answer corresponds with said category"));
           ("value", `Assoc (List.map (fun c -> (c, `Int 0)) cats))])

(** [q id cats] is a JSON representation of a question with answers
    of [cats] scoring categories*)
let q id cats = 
  `Assoc ([("id", `String id); 
           ("text", `String "Replace this text with your question.");
           ("answers", build_list ans_list [] cats "ans" 5)])

(** [qa_list cats num_qs] is a JSON representation of [num_qs] question and
    answer lists with [cats] scoring categories *)
let qa_list cats num_qs =
  let n = int_of_string num_qs in 
  build_list q [] cats "q" n

(** [build_quiz fname title desc sub cats num_qs] creates a JSON-formatted
    .quiz file *)
let build_quiz fname title desc sub cats num_qs =
  let j = `Assoc ([("title", `String title); ("desc", `String desc); 
                   ("subjective", `Bool sub); 
                   ("categories", (strings_to_json cats)); 
                   ("questions", (qa_list cats num_qs))]) in
  let file = open_out ("./quizzes/"^fname^".quiz") in
  Yojson.Basic.pretty_to_channel file j;
  close_out file

(** [prompt_name ()] is the file name of a new .quiz file that does not already 
    exist in the directory  *)
let rec prompt_name () = 
  print_string [] "Enter new .quiz file name > ";
  let f_check = read_line () in
  if Sys.file_exists ("./quizzes/"^f_check^".quiz") = false then f_check 
  else let () = print_string [yellow] 
           ("Sorry, a quiz with that filename already exists!\n") in
    prompt_name () 

(* [builder ()] prompts the user for details and executes the quiz builder *)
let builder () =
  print_string [Bold] "Create an ASKYQuiz!\n";
  let fname = prompt_name () in
  print_newline ();
  print_string [] "Enter quiz title > ";
  let title = read_line () in
  print_newline ();
  print_string [] "Enter quiz description > ";
  let desc = read_line () in
  print_newline ();
  print_string [] ("Will this quiz be (1) subjective e.g. personality quizzes "^
                   "or (2) non-subjective e.g. academic quizzes > ");
  let sub = if read_line () = "1" then true else false in
  print_newline ();
  let cats_list = if sub = true then
      let cats = (print_string [] ("What are the answer categories "^
                                   "(space-separated)? > ");
                  read_line ();) in String.split_on_char ' ' cats
    else ["correct"]; in
  print_newline ();
  print_string [] ("How many questions will this quiz have? > ");
  let num_qs = read_line () in
  print_newline (); 
  build_quiz fname title desc sub cats_list num_qs;
  ignore(Unix.system ("vim ./quizzes/"^ fname^".quiz"))
