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

(** Raised when quiz JSON contains invalid contents. *)
exception Invalid_quiz

(** [parse_json fn] is the [Quiz.t] represented by the JSON in file [fn]. *)
val parse_json : string -> (t, string) result

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

(** [answers qid t] is the list of answer choices for the question identified 
    by [qid] in [t].
    Raises [Not_found] if [qid] is not an id for a question in [t]. *)
val answers: t -> id -> answer list

(** [filename q] is the filename of the file containing data for [q]. *)
val filename: t -> string

(** [get_questions t] is a (id, qs) list for all questions in [t] *)
val get_questions: t -> (id * string) list

(** [get_answers qid t] is a (id, text) list for all answers of the question 
    associated with [qid] in [t] *)
val get_answers: id -> t -> (id * string) list

(** [get_txt_from_id qid t] is the [qs] of the question in t with id [qid].
    Raises [Not_found] if [qid] is not an id for a question in [t]. *)
val get_txt_from_id: id -> t -> string

(** [correct_ans answers] is the id of the correct answer in the list of answers
    Requires:
    - the corresponding quiz is non-subjective
    - there is only one correct answer *)
val correct_ans: answer list -> id

(** [get_values qid aid t] is the list of scores associated with 
    answer of id [aid] to question of id [qid] in [quiz].
    Raises [Not_found] if [aid] is not an id for an answer in question [qid]. *)
val get_values: id -> id -> t -> (category * int) list