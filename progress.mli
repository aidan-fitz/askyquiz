(** The type that represents progress while taking a quiz *)
type t

(** [init quiz] is the initial [progress] type for [quiz]*)
val init_progress: Quiz.t -> t

(** [update_progress qid aid quiz prog] updates [prog] to reflect the user
    answering with ID [aid] for question with ID [qid]. Running scores in 
    [prog] are updated according to the answer's scores in [quiz]. *)
val update_progress: Quiz.id -> Quiz.id -> Quiz.t -> t -> t

(** [queue q] is the list of ids of unanswered questions in quiz [q]. *)
val stock: t -> string list

(** [requeue q] is the list of ids of requeued questions in quiz [q]. *)
val discard: t -> string list

(** [score q] is the list of categories and corresponding scores in quiz [q]. *)
val score: t -> (string * int ref) list