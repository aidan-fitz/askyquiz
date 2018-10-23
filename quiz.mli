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

(** [questions_ids t] is the list of question ids from [Quiz.t]*)
val question_ids: t -> string list

(** [categories t] is the list of scoring categories from [Quiz.t]*)
val categories: t -> string list