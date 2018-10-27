(** The type that represents progress while taking a quiz *)
type t

(** [init quiz] is the initial [progress] type for [quiz]*)
val init_progress: Quiz.t -> t

(** [answered q] is the list of ids of answered questions in quiz [q]. *)
val answered: t -> string list

(** [answered q] is the list of ids of unanswered questions in quiz [q]. *)
val unanswered: t -> string list

(** [score q] is the list of categories and corresponding scores in quiz [q]. *)
val score: t -> (string * int ref) list

(** [best_category prog] is the category in [prog] with the highest score. *)
val best_category: t -> Quiz.category

(** [best_score prog] is the highest score of any category in [prog]. *)
val best_score: t -> int