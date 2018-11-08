open Quiz
open Progress

open OUnit2
open TestUtils
open TestUtils.ProgressUtils

let i = init_progress quiz1 (fun () -> Test)
let i_stock = stock i
(* The questions in i *)
let q1 = List.hd i_stock
let q2 = List.nth i_stock 1
let q3 = List.nth i_stock 2
let q4 = List.nth i_stock 3
let q5 = List.nth i_stock 4
(* Correct answer for question 1 *)
let a1 = q1 |> answers quiz1 |> correct_ans
let answers1 = get_answers q1 quiz1
(* Incorrect answer for q1 *)
let a1' = let a = fst (List.hd answers1) 
  in if a = a1 then fst(List.nth answers1 1) else a

let p1 = i |> pop_and_requeue true |> pop_and_requeue false |> pop_and_requeue 
           true
let p2 = p1 |> pop_and_requeue false |> pop_and_requeue true
let p3 = p2 |> pop_and_requeue false |> pop_and_requeue false |> pop_and_requeue
           false

let () = ignore (save_progress p1)
let p4 = get_progress quiz1 (fun () -> Test)

let progress_tests = [
  (* Tests for init_progress *)
  make_set_test "initial stock" i_stock ["q1"; "q2"; "q3"; "q4"; "q5"];
  make_set_test "initial discard pile" (discard i) [];
  make_set_test "initial score" (score i) [("correct", ref 0)];
  make_set_test "initial mastery level" (mastery i)
    [("q1", ref 0); ("q2", ref 0); ("q3", ref 0); ("q4", ref 0); ("q5", ref 0)];
  
  "filename" >:: (fun _ -> assert_equal "quiz1.prog" (filename i));
  "mode" >:: (fun _ -> assert_equal Test (quiz_mode i));
  
  (* Tests for next_question *)
  "next question" >:: (fun _ -> assert_equal (Some q1) (next_question i));
  "no next question" >:: (fun _ -> assert_equal None (next_question p3));

  (* Tests for pop_and_requeue *)
  make_set_test "pop and requeue stock" (stock p1) [q4; q5];
  make_set_test "pop and requeue discard" (discard p1) [q1; q3];
  make_set_test "stock after restock" (stock p2) [q1; q3; q5];
  "discard pile after restock" >:: (fun _ -> assert_equal [] (discard p2));

  (* Tests for update_scores *)
  make_set_test "score after incorrect ans" (score p2) [("correct", ref 0)];
  make_set_test "score after correct ans" (score p2) [("correct", ref 1)];
  
  (* Tests for save_progress and get_progress *)
  make_set_test "get progress stock" (stock p1) (stock p4);
  make_set_test "get progress discard" (discard p1) (discard p4);
  make_set_test "get progress score" (score p1) (score p4);
  make_set_test "get progress mastery" (mastery p1) (mastery p4);

  (* Tests for best_category and best_score *)
  "best category" >:: (fun _ ->
    assert_equal "correct" (best_category p2) ~printer:pp_string);
  "best score" >:: (fun _ ->
    assert_equal 1 (best_score p2) ~printer:string_of_int)
]
