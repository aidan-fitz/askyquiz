(** Progress of Quiz *)
open Quiz

type t = {
  stock: string list;
  discard: string list;
  score: (string * int ref) list
}

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle lst = QCheck.Gen.(generate1 (shuffle_l lst))

let init_progress quiz = {
  stock = quiz |> Quiz.question_ids |> shuffle;
  discard = [];
  score = List.map (fun x -> (x, ref 0)) (Quiz.categories quiz) 
}

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

(* let update_progress qid aid quiz prog =
  let scores = get_values qid aid quiz in
  (* update scores before copying to new record *)
  List.iter 
    (fun (category, score) ->
      let delta = List.assoc category scores in
      score := !score + delta)
    prog.score;
  {
    stock = List.tl prog.stock;
    discard = if do_requeue scores then qid :: prog.discard else prog.discard;
    score = prog.score;
  } |> restock *)

let stock t = t.stock

let discard t = t.discard

let score t = t.score

let best_category_data p =
  List.fold_left 
    (fun (max_cat, max_s) (cat, s) ->
      if !s > max_s then (cat, !s) else (max_cat, max_s))
    ("", 0)
    p.score

let best_category p = p |> best_category_data |> fst

let best_score p = p |> best_category_data |> snd