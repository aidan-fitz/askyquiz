(** The type that represents progress while taking a quiz *)
type t

(** The current quiz mode. *)
type mode = Subjective | Test | Practice

(** [init_progress quiz thunk] creates the initial state for [quiz], using
    [thunk] to compute the quiz mode. *)
val init_progress : Quiz.t -> (unit -> mode) -> t

(** [get_progress quiz thunk] loads the progress file corresponding to [quiz] 
    if it exists and if the user chooses to continue, and calls [init_progress 
    quiz thunk] otherwise. *)
val get_progress : Quiz.t -> (unit -> mode) -> t

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

(** [quiz_mode p] is the mode the user are playing in. *)
val quiz_mode : t -> mode

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
    file and then returns [p]. *)
val save_progress: t -> t