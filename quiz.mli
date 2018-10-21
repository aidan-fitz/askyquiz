(** Abstract type representing a quiz *)
type t

val check_answer : string -> t -> bool

module type Quiz = sig
    val display_results : t -> unit
end