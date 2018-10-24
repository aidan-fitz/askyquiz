open Progress

open OUnit2
open TestUtils

let i = init quiz1

let progress_tests = [
  (*TODO: writes tests for quiz*)
  "initial answered" >:: (fun  _ -> assert_equal (answered i) []);
  "initial unanswered" >:: (fun  _ -> assert_equal (unanswered i) 
                               ["q1"; "q2"; "q3"; "q4"; "q5"]);
  "initial score" >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists (score i) 
                          [("correct", ref 0)])
]

