open OUnit2
open Quiz
open Progress

open TestQuiz
open TestProgress

let suite = "quiz engine test suite" >::: 
            (quiz_tests @ progress_tests)

let _ = run_test_tt_main suite