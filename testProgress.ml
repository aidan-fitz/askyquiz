open Progress

open OUnit2
open TestUtils

let i = init_progress quiz1 (fun () -> Test)

let progress_tests = [
  (*TODO: writes tests for quiz*)
  "initial stock" >:: (fun  _ ->
    assert_equal (stock i) ["q1"; "q2"; "q3"; "q4"; "q5"]
    ~cmp:cmp_set_like_lists);
  "initial discard pile" >:: (fun _ -> assert_equal (discard i) []);
  "initial score" >:: (fun _ ->
    assert_equal (score i) [("correct", ref 0)]
    ~cmp:cmp_set_like_lists)
]

