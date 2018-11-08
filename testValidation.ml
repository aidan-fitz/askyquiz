open Validation

open OUnit2
open TestUtils
open TestUtils.ValidationUtils

let answer_options = [
  ("A", ("ans0", "discrete Fourier transform"));
  ("B", ("ans1", "discrete cosine transform"));
  ("C", ("ans2", "fast Fourier transform"));
  ("D", ("ans3", "discrete wavelet transform"))
]

let make_user_answer_test ans_in ans_out ans_options =
  (fun _ -> assert_equal (user_answer ans_in ans_options) ans_out)

let make_user_answer_test_ok ans_in ans_id ans_options = 
  make_user_answer_test ans_in (Ok ans_id) ans_options

let validation_tests = [
  "ans A" >:: make_user_answer_test_ok "A" "ans0" answer_options;
  "ans B" >:: make_user_answer_test_ok "B" "ans1" answer_options;
  "ans C" >:: make_user_answer_test_ok "C" "ans2" answer_options;
  "ans D" >:: make_user_answer_test_ok "D" "ans3" answer_options;
  "ans a" >:: make_user_answer_test_ok "a" "ans0" answer_options;
  "ans b" >:: make_user_answer_test_ok "b" "ans1" answer_options;
  "ans c" >:: make_user_answer_test_ok "c" "ans2" answer_options;
  "ans d" >:: make_user_answer_test_ok "d" "ans3" answer_options;
]
