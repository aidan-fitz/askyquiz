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

(** [title t] is the title of [Quiz.t] *)
val title: t -> string

(** [desc t] is the description of [Quiz.t] *)
val desc: t -> string

(** [categories t] is the list of scoring categories from [Quiz.t]*)
val categories: t -> string list

(** [questions_ids t] is the list of question ids from [Quiz.t]*)
val question_ids: t -> string list

(** [question_texts t] is the list of question texts from [Quiz.t] *)
val question_texts: t -> string list

(** [categories t] is the list of scoring categories from [Quiz.t]*)
val categories: t -> string list

(** [get_values qid aid quiz] is the list of scores associated with 
    answer of id [aid] to question of id [qid] in [quiz]. *)
val get_values: string -> string -> t -> (category * int) list

val get_answers: string -> t -> (string * string) list

val get_questions: t -> (string * string) list