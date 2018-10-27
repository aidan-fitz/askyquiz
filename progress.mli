(** The type that represents progress while taking a quiz *)
type t

(** [init quiz] is the initial [progress] type for [quiz]*)
val init_progress: Quiz.t -> t

(** [queue q] is the list of ids of unanswered questions in quiz [q]. *)
val queue: t -> string list

(** [requeue q] is the list of ids of requeued questions in quiz [q]. *)
val requeue: t -> string list

(** [score q] is the list of categories and corresponding scores in quiz [q]. *)
val score: t -> (string * int ref) list