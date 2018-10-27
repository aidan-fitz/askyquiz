(** Progress of Quiz *)
open Quiz

type t = {
  stock: string list;
  discard: string list;
  score: (string * int ref) list
}

let init_progress quiz = {
  stock = Quiz.question_ids quiz;
  discard = [];
  score = List.map (fun x -> (x, ref 0)) (Quiz.categories quiz) 
}

(** [do_requeue s] tells whether to discard the most recently answered question
    based on [s]. *)
let do_requeue scores =
  (* this is None if scores is for a personality quiz *)
  List.assoc_opt "correct" scores = Some 0

let restock prog =
  if prog.stock = [] then {prog with stock = prog.discard; discard = []}
  else prog

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