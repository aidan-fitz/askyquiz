open Yojson
open Yojson.Basic
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

exception Invalid_quiz

(** [build_answers j] is a list of answers parsed from the JSON [j] for
    a specific question.
    Raises [Invalid_quiz] if list of answers is empty. *)
let build_answers j =
  let ans = List.map (fun a -> 
      {
        id = str_mem "id" a;
        text = str_mem "text" a;
        values = 
          List.map 
            (fun (c, i) -> (c, i |> to_int))
            (a |> member "value" |> to_assoc)
      }) 
    (j |> to_list)
  in if ans = [] then raise Invalid_quiz else ans

(** [build_questions j] is a list of all questions parsed from the JSON [j]. *)
let build_questions j =
  let qs = 
    List.map (fun q -> 
      {
        id = str_mem "id" q;
        qs = str_mem "text" q;
        answers = build_answers (q |> member "answers")
      })
    (lst_mem "questions" j)
  in if qs = [] then raise Invalid_quiz else qs

let parse_json fn =
  try let j = Yojson.Basic.from_file fn in
  Ok {
    title = str_mem "title" j;
    desc = str_mem "desc" j;
    subjective = j |> member "subjective" |> to_bool;
    categories = List.map to_string (lst_mem "categories" j);
    questions = build_questions j;
    filename = fn
  }
  with
    | Sys_error _  -> Error "File not found."
    | Json_error _ -> Error "File has invalid JSON."
    | Type_error _
    | Invalid_quiz -> Error "JSON doesn't represent quiz."

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
  List.map (fun x -> x.id, x.qs) t.questions

let get_answers qid t =
  List.map (fun (x : answer) -> (x.id, x.text)) (answers t qid)

let rec correct_ans = function
  (* find ans s.t. it has one value with score 1 *)
  | ans :: tail -> begin
      match ans.values with
        | [] -> failwith "answer has no values"
        | [("correct", v)] -> if v = 1 then ans.id else correct_ans tail
        | [(_, _)] -> failwith "incorrect format"
        | _ -> failwith "subjective quiz"
      end
  (* end of list *)
  | [] -> failwith "no correct answer"

let get_values qid aid t =
  (List.find (fun {id; text; _} -> id = aid) (answers t qid)).values
