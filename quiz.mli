(** The type of each scoring categories. *)
type category = string

(** The type of question in a quiz. *)
type question

(** The type of response to a question. *)
type answer

(** Abstract type representing a quiz *)
type t

(** [parse_json j] is the [Quiz.t] represented by the JSON [j]. *)
val parse_json : Yojson.Basic.json -> t