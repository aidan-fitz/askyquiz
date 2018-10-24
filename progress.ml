(** Progress of Quiz *)
open Quiz

(*Make initial Progress Module from quiz type*)

(* module type Prog = sig
   type t 
   val init: t
   end

   module Progress = struct
   type t  = {answered: string list; 
             unanswered: string list;
             score: (string * int) list}

   let init = {
    answered = [];
    unanswered = [];(* all question ids *)
    score = [("category",0)]
   }
   end *)

(* Play functor takes initial Progress module and makes next Progress module*)
(* module Update (P:Prog) : Prog = struct
   include Progress
   end *)

type t = {
  answered: string list; 
  unanswered: string list;
  score: (string * int ref) list
}

let init quiz = {
  answered = [];
  unanswered = Quiz.question_ids quiz;
  score = List.map (fun x -> (x, ref 0)) (Quiz.categories quiz) 
}

(** [update_progress qid aid quiz prog] changes [prog] to reflect the user
    answering with ID [aid] for question with ID [qid]. Running scores in 
    [prog] are updated according to the scores in [quiz]. *)
let update_progress qid aid quiz prog =
  let values = get_values qid aid quiz in
  List.iter
    (fun (c, i) ->
       let (cat, score) = List.find (fun (x, y) -> x=c) values in
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