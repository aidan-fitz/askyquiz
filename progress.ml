(** Progress of Quiz *)
open Quiz

type t = {
  answered: string list; 
  unanswered: string list;
  score: (string * int ref) list
}

let init_progress quiz = {
  answered = [];
  unanswered = Quiz.question_ids quiz;
  score = List.map (fun x -> (x, ref 0)) (Quiz.categories quiz) 
}

(** [update_progress qid aid quiz prog] updates [prog] to reflect the user
    answering with ID [aid] for question with ID [qid]. Running scores in 
    [prog] are updated according to the answer's scores in [quiz]. *)
let update_progress qid aid quiz prog =
  let values = get_values qid aid quiz in
  (* update scores before copying to new record *)
  List.iter 
    (fun (c, i) ->
      let (cat, score) = List.find (fun (x, _) -> x=c) values in
      i := score + !i)
    prog.score;
  {
    answered = qid :: prog.answered;
    unanswered = List.filter ((<>) qid) prog.unanswered;
    score = prog.score;
  }

let answered t = t.answered

let unanswered t = t.unanswered

let score t = t.score