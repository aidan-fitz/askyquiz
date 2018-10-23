open Yojson.Basic.Util

type category = string

(** [answer] represents a single answer option for any question, with a 
    corresponding string [id], displayed [text] and  *)
type answer = {
  id: string;
  text: string;
  values: (string * int) list
}

(** [question] represents a question with a string [id], displayed [text], and
    a list of possible [answers]. *)
type question = {
  id: string;
  text: string;
  answers: answer list
}

(** [Quiz.t] represents a single quiz with a title [title], description [desc],
    scoring [categories], and a list of [questions]. *)
type t = {
  title: string;
  desc: string;
  categories: string list;
  questions: question list
}

(** Possible helper method *)
let shuffle lst = ()

(** [get_field_str key json] is the string value associated with [key] in 
    [json].
    Requires: [key] is associated with a string value. *)
let str_field key json = json |> member key |> to_string

(** [build_answers aj] is a list of answers parsed from the JSON [aj] for
    a specific question. *)
let build_answers aj =
  List.map (fun a -> {
        id = str_field "id" a;
        text = str_field "text" a;
        values = 
          List.map 
            (fun (c, i) -> (c, i |> to_int))
            (a |> member "value" |> to_assoc)
      }) (aj |> to_list)

(** [build_questions j] is a list of all questions parsed from the JSON [j]. *)
let build_questions j =
  let qs = j |> member "questions" |> to_list in
  List.map (fun q -> {
        id = str_field "id" q;
        text = str_field "text" q;
        answers = build_answers (q |> member "answers")
      }) qs

let parse_json j =
  {
    title = j |> member "title" |> to_string;
    desc = j |> member "desc" |> to_string;
    categories = List.map (fun c -> c |> to_string) 
        (j |> member "categories" |> to_list);
    questions = build_questions j
  }

let title t = t.title

let desc t = t.desc

let categories t = t.categories

let question_ids t = List.map (fun x -> x.id) t.questions

let question_texts t = List.map (fun x -> x.text) t.questions
