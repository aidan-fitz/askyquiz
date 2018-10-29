open Yojson.Basic.Util

type category = string
type id = string

(** [answer] represents a single answer option for any question, with a 
    corresponding [id], displayed [text] and a list of scores it
    contributes to various scoring axes. *)
type answer = {
  id: id;
  text: string;
  values: (string * int) list
}

(** [question] represents a question with ID [id], displayed [text], and
    a list of possible [answers]. *)
type question = {
  id: id;
  qs: string;
  answers: answer list
}

(** [Quiz.t] represents a single quiz with a title [title], description [desc],
    scoring [categories], and a list of [questions]. *)
type t = {
  title: string;
  desc: string;
  subjective: bool;
  categories: string list;
  questions: question list;
  filename: string
}

(** [str_mem key json] is the string value associated with [key] in [json].
    Requires: [key] is associated with a string value. *)
let str_mem key json = json |> member key |> to_string

(** [lst_mem key json] is the list associated with [key] in [json].
    Requires: [key] is associated with a string value. *)
let lst_mem key json = json |> member key |> to_list

(** [build_answers j] is a list of answers parsed from the JSON [j] for
    a specific question. *)
let build_answers j =
  List.map (fun a -> 
      {
        id = str_mem "id" a;
        text = str_mem "text" a;
        values = 
          List.map 
            (fun (c, i) -> (c, i |> to_int))
            (a |> member "value" |> to_assoc)
      }) 
    (j |> to_list)

(** [build_questions j] is a list of all questions parsed from the JSON [j]. *)
let build_questions j =
  let qs = j |> member "questions" |> to_list in
  List.map (fun q -> 
      {
        id = str_mem "id" q;
        qs = str_mem "text" q;
        answers = build_answers (q |> member "answers")
      })
    qs

let parse_json fn =
  let j = Yojson.Basic.from_file fn in
  {
    title = str_mem "title" j;
    desc = str_mem "desc" j;
    subjective = j |> member "subjective" |> to_bool;
    categories = List.map to_string (lst_mem "categories" j);
    questions = build_questions j;
    filename = fn
  }

let get_q_from_id qid t = List.find (fun {id; qs; _} -> id = qid) t.questions

let get_txt_from_id qid t = (get_q_from_id qid t).qs

let title t = t.title

let desc t = t.desc

let subjective t = t.subjective

let categories t = t.categories

let question_ids t = List.map (fun x -> x.id) t.questions

let question_qs t = List.map (fun x -> x.qs) t.questions

let answers t qid = (get_q_from_id qid t).answers

let filename q = q.filename

let get_questions t = 
  let rec pair lst1 lst2 acc = 
    match (lst1, lst2) with 
    | (h1 :: t1, h2 :: t2) -> pair t1 t2 ((h1, h2) :: acc)
    | _ -> acc
  in pair (question_ids t) (question_qs t) []

let get_answers qid t =
  let q = List.find (fun {id; qs; _} -> id = qid) t.questions in
  let a = q.answers in
  List.map (fun (x : answer) -> (x.id, x.text)) a

let rec correct_ans = function
  | h :: t -> if begin 
    (match h.values with
     | (c, v) :: [] -> v = 1
     | _ -> failwith "answer has no values"
    ) end then h.id else correct_ans t
  | _ -> failwith "no correct answer"

let get_values qid aid t =
  let question = get_q_from_id qid t in
  let answer   = List.find (fun {id; text; _} -> id = aid) question.answers in
  answer.values
