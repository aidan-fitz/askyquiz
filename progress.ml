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

type progress  = {answered: string list; 
                  unanswered: string list;
                  score: (string * int ref) list}

let init quiz = {
  answered = [];
  unanswered = Quiz.question_ids quiz;
  score = List.map (fun x -> (x, ref 0)) (Quiz.categories quiz) 
}
