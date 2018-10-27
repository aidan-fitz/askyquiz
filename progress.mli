(** The type that represents progress while taking a quiz *)
type t

(** [init quiz] is the initial [progress] type for [quiz]*)
val init_progress: Quiz.t -> t

(** [update_progress qid aid quiz prog] updates [prog] to reflect the user
    answering with ID [aid] for question with ID [qid]. Running scores in 
    [prog] are updated according to the answer's scores in [quiz]. *)
val update_scores: Quiz.id -> Quiz.id -> Quiz.t -> t -> t

(** [queue q] is the list of ids of unanswered questions in quiz [q]. *)
val stock: t -> string list

(** [requeue q] is the list of ids of requeued questions in quiz [q]. *)
val discard: t -> string list

(** [score q] is the list of categories and corresponding scores in quiz [q]. *)
val score: t -> (string * int ref) list

(** [next_question prog] is the next question to be asked. *)
val next_question: t -> Quiz.id option

(** [pop_and_requeue rq prog] is the resulting [Progress.t] after popping
    and adding a question to the discard pile. *)
val pop_and_requeue: bool -> t -> t