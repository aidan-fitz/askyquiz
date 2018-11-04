(** The type that represents progress while taking a quiz *)
type t

(** [init_progress quiz] is the initial [progress] type for [quiz]*)
val init_progress : Quiz.t -> t

(** [get_progress quiz] loads the progress file corresponding to [quiz] if it
    exists, and calls [init_progress quiz] otherwise. *)
val get_progress : Quiz.t -> t

(** [update_progress qid aid quiz prog] updates [prog] to reflect the user
    answering with ID [aid] for question with ID [qid]. Running scores in 
    [prog] are updated according to the answer's scores in [quiz]. *)
val update_scores: Quiz.id -> Quiz.id -> Quiz.t -> t -> unit

(** [queue q] is the list of ids of unanswered questions in quiz [q]. *)
val stock: t -> string list

(** [requeue q] is the list of ids of requeued questions in quiz [q]. *)
val discard: t -> string list

(** [score q] is the list of categories and corresponding scores in quiz [q]. *)
val score: t -> (string * int ref) list

(** [mastery p] are the mastery scores for the questions in the progress 
    state [p]. *)
val mastery: t -> (string * int ref) list

(** [filename p] is the path to the progress file associated with [p]. *)
val filename : t -> string

(** [next_question prog] is the next question to be asked. *)
val next_question: t -> Quiz.id option

(** [pop_and_requeue rq prog] is the resulting [Progress.t] after popping
    and adding a question to the discard pile. *)
val pop_and_requeue: bool -> t -> t

(** [best_category prog] is the category in [prog] with the highest score. *)
val best_category: t -> Quiz.category

(** [best_score prog] is the highest score of any category in [prog]. *)
val best_score: t -> int

(** [save_progress p] writes the contents of [p] to its associated progress 
    file. *)
val save_progress: t -> unit