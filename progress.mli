(** The type that represents progress while taking a quiz *)
type t

(** [init quiz] is the initial [progress] type for [quiz]*)
val init: Quiz.t -> t

(** [answered t] is the list of ids of answered questions in the corresponding 
    quiz *)
val answered: t -> string list

(** [answered t] is the list of ids of unanswered questions in the corresponding
    quiz *)
val unanswered: t -> string list

(** [score t] is the list of categories and corresponding scores *)
val score: t -> (string * int ref) list