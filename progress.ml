(** Progress of Quiz *)
open Quiz

type t = {
  queue: string list;
  requeue: string list;
  score: (string * int ref) list
}

let init_progress quiz = {
  queue = Quiz.question_ids quiz;
  requeue = [];
  score = List.map (fun x -> (x, ref 0)) (Quiz.categories quiz) 
}

(** [update_progress qid aid quiz prog] updates [prog] to reflect the user
    answering with ID [aid] for question with ID [qid]. Running scores in 
    [prog] are updated according to the answer's scores in [quiz]. *)
let update_progress qid aid quiz prog =
  let scores = get_values qid aid quiz in
  (* update scores before copying to new record *)
  List.iter 
    (fun (category, score) ->
      let delta = List.assoc category scores in
      score := !score + delta)
    prog.score;
  {
    queue = List.filter ((<>) qid) prog.queue;
    requeue = qid :: prog.requeue; (* TODO add logic for requeuing if incorrect *)
    score = prog.score;
  }

let queue t = t.queue

let score t = t.score