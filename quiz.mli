(** The type of each scoring category. *)
type category = string

(** The type of id associated with each question and answer option. *)
type id = string

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

(** [subjective t] is whether the corresponding quiz is subjective *)
val subjective: t -> bool

(** [categories t] is the list of scoring categories from [Quiz.t]*)
val categories: t -> string list

(** [questions_ids t] is the list of question ids from [Quiz.t]*)
val question_ids: t -> id list

(** [question_texts t] is the list of question texts from [Quiz.t] *)
val question_qs: t -> string list

(** [categories t] is the list of scoring categories from [Quiz.t]*)
val categories: t -> string list

(** [get_questions t] is a (id, qs) list for all questions in [t] *)
val get_questions: t -> (id * string) list

(** [get_answers qid t] is a (id, text) list for all answers of the question 
    associated with [qid] in [t] *)
val get_answers: string -> t -> (id * string) list

(** [get_values qid aid t] is the list of scores associated with 
    answer of id [aid] to question of id [qid] in [quiz]. *)
val get_values: id -> id -> t -> (category * int) list