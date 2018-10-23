(** Abstract type representing a quiz *)
type t
type question
type answer

type question

val check_answer : string -> t -> bool

module type Quiz = sig
    val display_results : t -> unit
end

val parse_json : Yojson.Basic.json -> t

(**
Ok, so...what should a quiz do?

When using the program:
- First prompts for a question file
- Then loads in file and parses into quiz structure (which has something)
- Print out questions and answers (so we have a list of them somewhere)
- Each question has:
  - Text
  - Answer options, each option has:
    - Text
    - Assoc list between categories and points (ints)

What parts of the types should be abstracted?


*)

module type Q = sig
    (** Always returns true for subjective quizzes *)
    val check_answer : string -> t -> bool
    
end