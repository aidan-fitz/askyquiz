open Progress

open OUnit2
open TestUtils

let i = init_progress quiz1

let progress_tests = [
  (*TODO: writes tests for quiz*)
  "initial unanswered" >:: (fun  _ -> assert_equal (queue i) 
                               ["q1"; "q2"; "q3"; "q4"; "q5"]);
  "initial requeued" >:: (fun _ -> assert_equal (requeue i) []);
  "initial score" >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists (score i) 
                          [("correct", ref 0)])
]

