open OUnit2
open Quiz

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

let quiz1 = parse_json (Yojson.Basic.from_file "quiz1.json")
let hp = parse_json (Yojson.Basic.from_file "hp.json")

let quiz_tests = [
  (*TODO: writes tests for quiz*)

  (* Testing parse_json *)
  "title" >:: (fun _ -> assert_equal (title hp) 
                  "Which Hogwarts house are you in?");
  "description" >:: (fun _ -> assert_equal (desc hp) 
                        "This is an example personality quiz! Source: \
                         https://www.proprofs.com/quiz-school/story.php?\
                         title=harry-potter-house-test_2");
  "categories" >:: (fun _ -> assert_equal (categories hp) 
                       ["Gryffindor"; "Hufflepuff"; "Ravenclaw"; "Slytherin"]);
  "question ids" >:: (fun _ -> assert_equal (question_ids hp) 
                         ["q1"; "q2"; "q3"; "q4"; "q5"]);
  "2nd question text" >:: (fun _ -> assert_equal (List.nth (question_qs hp) 1) 
                              "How would you react if someone picks on you and \
                               your friend?");
  "3rd question text" >:: (fun _ -> assert_equal 
                              (List.nth (question_qs hp) 2) 
                              "What's your favorite color?");
  "1st q answer id/text" >:: (fun _ -> assert_equal 
                                 (get_answers (List.hd (question_ids hp)) hp) 
                                 [("q1a1", "Save myself, of course!"); 
                                  ("q1a2", "Run and grab my friend that is \
                                            still in the building"); 
                                  ("q1a3", "It depends, if I have a way to \
                                            save my friend, than yes, but if \
                                            there's no way then I am above \
                                            everybody else."); 
                                  ("q1a4", "I'll try, but I am not sure!")]);
]

let progress_tests = [
  (*TODO: writes tests for quiz*)
]

let suite = "quiz engine test suite" >::: 
            (quiz_tests @ progress_tests)

let _ = run_test_tt_main suite