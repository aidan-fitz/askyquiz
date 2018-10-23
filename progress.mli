(** The type that represents progress while taking a quiz *)
type progress 

(** [init quiz] is the initial [progress] type for [quiz]*)
val init: Quiz.t -> progress