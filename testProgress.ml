open Quiz
open Progress

open OUnit2
open TestUtils

let (>::) name f = ("Progress: " ^ name) >:: f

let i = init_progress quiz1 (fun () -> Test)
let i_stock = stock i
(* The questions in i *)
let q1 = List.hd i_stock
let q2 = List.nth i_stock 1
let q3 = List.nth i_stock 2
let q4 = List.nth i_stock 3
let q5 = List.nth i_stock 4

let p1 = i |> pop_and_requeue true |> pop_and_requeue false |> pop_and_requeue 
           true
let p2 = p1 |> pop_and_requeue false |> pop_and_requeue true

let p3 = (init_progress quiz1 (fun () -> Test)) |> pop_and_requeue false |> 
         pop_and_requeue false |> pop_and_requeue false
let q1' = List.hd (stock p3)
let a1 = q1' |> answers quiz1 |> correct_ans
let answers1 = get_answers q1' quiz1
let a1' = let a = fst (List.hd answers1) 
  in if a = a1 then fst (List.nth answers1 1) else a

let p4 = p3 |> pop_and_requeue false |> pop_and_requeue false

let () = ignore (save_progress p1)
let p5 = get_progress quiz1 (fun () -> Test)

let progress_tests = [
  (*TODO: writes tests for quiz*)
  (* Tests for init_progress *)
  "initial stock" >:: (fun  _ ->
      assert_equal ["q1"; "q2"; "q3"; "q4"; "q5"] i_stock
        ~cmp:cmp_set_like_lists);
  "initial discard pile" >:: (fun _ -> assert_equal [] (discard i));
  "initial score" >:: (fun _ ->
      assert_equal [("correct", ref 0)] (score i)
        ~cmp:cmp_set_like_lists);
  "initial mastery level" >:: (fun _ -> assert_equal 
                                  [("q1", ref 0); ("q2", ref 0); ("q3", ref 0); 
                                   ("q4", ref 0); ("q5", ref 0)] (mastery i));
  "filename" >:: (fun _ -> assert_equal "quiz1.prog" (filename i));
  "mode" >:: (fun _ -> assert_equal Test (quiz_mode i));
  (* Tests for next_question *)
  "next question" >:: (fun _ -> assert_equal (Some q1) (next_question i));
  "no next question" >:: (fun _ -> assert_equal None (next_question p4));
  (* Tests for pop_and_requeue *)
  "pop and requeue stock" >:: (fun _ -> assert_equal [q4; q5] (stock p1) 
                                  ~cmp:cmp_set_like_lists);
  "pop and requeue discard" >:: (fun _ -> assert_equal [q1; q3] (discard p1)
                                    ~cmp:cmp_set_like_lists);
  "stock after restock" >:: (fun _ -> assert_equal [q1; q3; q5] (stock p2) 
                                ~cmp:cmp_set_like_lists);
  "discard pile after restock" >:: (fun _ -> assert_equal [] (discard p2));
  (* Tests for update_scores *)
  "score after incorrect ans" >:: (fun _ -> 
      update_scores q1' a1' quiz1 p3; 
      assert_equal [("correct", ref 0)] (score p3));
  "score after correct ans" >:: (fun _ -> 
      update_scores q1' a1 quiz1 p3; 
      assert_equal [("correct", ref 1)] (score p3));
  (*Tests for save_progress and get_progress *)
  "get progress stock" >:: (fun _ -> assert_equal (stock p5) (stock p1));
  "get progress discard" >:: (fun _ -> assert_equal (discard p5) (discard p1));
  "get progress score" >:: (fun _ -> assert_equal (score p5) (score p1));
  "get progress mastery" >:: (fun _ -> assert_equal (mastery p5) (mastery p1));
  (* Tests for best_category and best_score *)
  "best category" >:: (fun _ -> assert_equal "correct" (best_category p3));
  "best score" >:: (fun _ -> assert_equal 1 (best_score p3) ~printer:string_of_int)
]

