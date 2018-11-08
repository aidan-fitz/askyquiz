open OUnit2
(* Common variables for testing *)

(** [quiz_from_file fn] parses the contents of [fn] into a [Quiz.t].
    Raises [Failure] if [fn] does not represent a [Quiz.t]. *)
let quiz_from_file fn =
  match Quiz.parse_json fn with
  | Ok q -> q
  | Error msg -> failwith msg

(* Create quizzes to test with. *)
let quiz1 = quiz_from_file "quiz1.quiz"
let hp = quiz_from_file "hp.quiz"
let bread = quiz_from_file "quizzes/bread.quiz"
let demo = quiz_from_file "quizzes/demo.quiz"
let pottermore = quiz_from_file "quizzes/pottermore.quiz"

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let pp_score (c, s) =
  Printf.sprintf "(%s, %d)" c !s

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(************ CUSTOM TEST MAKERS ************)
module Make (T:sig val prefix: string end ) = struct
  (** Prepends the prefix. *)
  let prefixify name = T.prefix ^ ": " ^ name

  let (>::) name f = (prefixify name) >:: f

  let make_set_test
      (name:string)
      (lst:'a list)
      (expected:'a list) =
    name >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists expected lst)
end

module BuilderUtils = Make(struct
  let prefix = "Builder"
end)
module ProgressUtils = Make(struct
  let prefix = "Progress"
end)
module QuizUtils = Make(struct
  let prefix = "Quiz"
end)
module ValidationUtils = Make(struct
  let prefix = "Validation"
end)
