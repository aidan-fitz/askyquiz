open Quiz
open Progress

open OUnit2
open TestUtils

let i = init_progress quiz1 (fun () -> Test)
let i_stock = stock i
(* The questions in i *)
let q1 = List.hd i_stock
let q2 = List.nth i_stock 1
let q3 = List.nth i_stock 2
let q4 = List.nth i_stock 3
let q5 = List.nth i_stock 4
(* Correct answer for q1*)
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
  (*TODO: writes tests for quiz*)
  (* Tests for init_progress *)
  "initial stock" >:: (fun  _ ->
      assert_equal i_stock ["q1"; "q2"; "q3"; "q4"; "q5"]
        ~cmp:cmp_set_like_lists);
  "initial discard pile" >:: (fun _ -> assert_equal (discard i) []);
  "initial score" >:: (fun _ ->
      assert_equal (score i) [("correct", ref 0)]
        ~cmp:cmp_set_like_lists);
  "initial mastery level" >:: (fun _ -> assert_equal (mastery i) 
                                  [("q1", ref 0); ("q2", ref 0); ("q3", ref 0); 
                                   ("q4", ref 0); ("q5", ref 0)]);
  "filename" >:: (fun _ -> assert_equal (filename i) "quiz1.prog");
  "mode" >:: (fun _ -> assert_equal (quiz_mode i) Test);
  (* Tests for next_question *)
  "next question" >:: (fun _ -> assert_equal (next_question i) (Some q1));
  "no next question" >:: (fun _ -> assert_equal (next_question p3) None);
  (* Tests for pop_and_requeue *)
  "pop and requeue stock" >:: (fun _ -> assert_equal (stock p1) [q4; q5] 
                                  ~cmp:cmp_set_like_lists);
  "pop and requeue discard" >:: (fun _ -> assert_equal (discard p1) [q1; q3]
                                    ~cmp:cmp_set_like_lists);
  "stock after restock" >:: (fun _ -> assert_equal (stock p2) [q1; q3; q5] 
                                ~cmp:cmp_set_like_lists);
  "discard pile after restock" >:: (fun _ -> assert_equal (discard p2) []);
  (* Tests for update_scores *)
  "score after incorrect ans" >:: (fun _ -> 
      update_scores q1 a1' quiz1 p2; 
      assert_equal (score p2) [("correct", ref 0)]);
  "score after correct ans" >:: (fun _ -> 
      update_scores q1 a1 quiz1 p2; 
      assert_equal (score p2) [("correct", ref 1)]);
  (*Tests for save_progress and get_progress *)
  "get progress stock" >:: (fun _ -> assert_equal (stock p1) (stock p4));
  "get progress discard" >:: (fun _ -> assert_equal (discard p1) (discard p4));
  "get progress score" >:: (fun _ -> assert_equal (score p1) (score p4));
  "get progress mastery" >:: (fun _ -> assert_equal (mastery p1) (mastery p4));
  (* Tests for best_category and best_score *)
  "best category" >:: (fun _ -> assert_equal (best_category p2) "correct" ~printer:pp_string);
  "best score" >:: (fun _ -> assert_equal (best_score p2) 1)
]

