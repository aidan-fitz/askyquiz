(** Progress of Quiz *)
open Quiz
open Yojson.Basic.Util

type mode = Subjective | Test | Practice

type t = {
  stock: string list;
  discard: string list;
  score: (string * int ref) list;
  mastery: (string * int ref) list;
  filename: string;
  mode: mode
}

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle lst = QCheck.Gen.(generate1 (shuffle_l lst))

(** [save_filename fn] is [<file>.prog] where [fn] is [<file>.quiz]
    Requires: [fn] has the [.quiz] extension. *)
let save_filename fn =
  let match_ext = Str.regexp "\\.quiz$" in 
  Str.replace_first match_ext ".prog" fn

(** [do_requeue s] tells whether to discard the most recently answered question
    based on [s]. *)
let do_requeue scores =
  (* this is None if scores is for a personality quiz *)
  List.assoc_opt "correct" scores = Some 0

(** [restock prog] is the resulting [Progress.t] from moving questions in the
    discard pile to the current queue. *)
let restock prog =
  if prog.stock = [] then {prog with stock = shuffle prog.discard; discard = []}
  else prog

(** [pop_current_question prog] is [current_question, prog'] where [prog'] is
  * [prog] with [current_question] removed. *)
let pop_current_question prog =
  match prog.stock with
  | [] -> invalid_arg "empty stock"
  | h :: t -> h, {prog with stock = t}

(** [requeue qid prog] is the resulting [Progress.t] after adding [qid] to the
    discard pile. *)
let requeue qid prog =
  {prog with discard = qid :: prog.discard}

(************** BUILD JSON HELPERS **************)
(** [strings_to_json ls] is a JSON representation of [ls]. *)
let strings_to_json (ls: string list) = `List (List.map (fun s -> `String s) ls)

(** [scores_to_json ls] is a JSON representation of [ls], an association list
    between strings and mutable ints. *)
let scores_to_json (ls: (string * int ref) list) =
  `Assoc (List.map (fun (c, s) -> (c, `Int !s)) ls)

(** [mode_to_json mode] is a JSON representation of [mode]. *)
let mode_to_json = function
  | Subjective -> `String "subjective"
  | Practice -> `String "practice"
  | Test -> `String "test"

(************** PARSE JSON HELPERS **************)
(** [build_strings field j] is the list of strings corresponding to [field] in
    JSON [j].
    Requires: [field] corresponds to a list of strings in [j]. *)
let build_strings field j =
  j |> member field |> to_list |> List.map to_string

(** [build_string_refs field j] is an association list of strings to mutable
    ints corresponding to [field] in JSON [j].
    Requires: [field] contains string to int mappings in [j]. *)
let build_string_refs field j =
  let lst = j |> member field |> to_assoc in
  List.map (fun (c, j) -> (c, j |> to_int |> ref)) lst

let build_mode j =
  match j |> member "mode" |> to_string with
  | "subjective" -> Subjective
  | "practice" -> Practice
  | "test" -> Test
  | _ -> failwith "invalid mode"

(** [load_progress filename] is a [Progress.t] constructed from the JSON in
    [filename]. *)
let load_progress filename =
  let j = Yojson.Basic.from_file filename in
  {
    stock = build_strings "stock" j;
    discard = build_strings "discard" j;
    score = build_string_refs "score" j;
    mastery = build_string_refs "mastery" j;
    filename = filename;
    mode = build_mode j
  }

(************** INTERFACE FUNCTIONS *************)
let init_progress quiz compute_mode = {
  stock = quiz |> Quiz.question_ids |> shuffle;
  discard = [];
  score = List.map (fun cat -> (cat, ref 0)) (Quiz.categories quiz);
  mastery = List.map (fun id -> (id, ref 0)) (question_ids quiz);
  filename = save_filename (filename quiz);
  mode = compute_mode ()
}

let get_progress quiz compute_mode =
  let prog_file = save_filename (filename quiz) in 
  if Sys.file_exists prog_file then
    load_progress prog_file
  else
    init_progress quiz compute_mode

let update_scores qid aid quiz prog =
  let scores = get_values qid aid quiz in
  (* update scores before copying to new record *)
  List.iter 
    (fun (category, score) ->
      let delta = List.assoc category scores in
      score := !score + delta)
    prog.score

let next_question prog =
  match prog.stock with
  | [] -> None
  | h :: _ -> Some h

let pop_and_requeue rq prog =
  let last_q, prog' = pop_current_question prog in 
  (if rq then requeue last_q prog' else prog') |> restock

let stock t = t.stock

let discard t = t.discard

let score t = t.score

let mastery p = p.mastery

let filename p = p.filename

let quiz_mode p = p.mode

let best_category_data p =
  List.fold_left 
    (fun (max_cat, max_s) (cat, s) ->
      if !s > max_s then (cat, !s) else (max_cat, max_s))
    ("", 0)
    p.score

let best_category p = p |> best_category_data |> fst

let best_score p = p |> best_category_data |> snd

let save_progress p =
  let j = `Assoc ([
    ("mode", mode_to_json p.mode);
    ("stock", strings_to_json p.stock);
    ("discard", strings_to_json p.discard);
    ("score", scores_to_json p.score);
    ("mastery", scores_to_json p.mastery)
  ]) in
  Yojson.Basic.to_file p.filename j; p
