open OUnit2
open Quiz
open Progress

open TestUtils

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
                                  ("q1a2", "Run and grab my friend who's \
                                            still in the building"); 
                                  ("q1a3", "It depends -- if I have a way to \
                                            save my friend, than yes, but if \
                                            there's no way then I am above \
                                            everybody else."); 
                                  ("q1a4", "I'll try, but I am not sure!")]);
]

let i = init quiz1

let progress_tests = [
  (*TODO: writes tests for quiz*)
  "initial answered" >:: (fun  _ -> assert_equal (answered i) []);
  "initial unanswered" >:: (fun  _ -> assert_equal (unanswered i) 
                               ["q1"; "q2"; "q3"; "q4"; "q5"]);
  "initial score" >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists (score i) 
                          [("correct", ref 0)])
]

let suite = "quiz engine test suite" >::: 
            (quiz_tests @ progress_tests)

let _ = run_test_tt_main suite