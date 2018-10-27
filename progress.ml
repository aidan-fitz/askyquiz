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
  if prog.stock = [] then
    {prog with stock = prog.discard; discard = []}
  else
    prog

let update_progress qid aid quiz prog =
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
  } |> restock

let stock t = t.stock

let discard t = t.discard

let score t = t.score