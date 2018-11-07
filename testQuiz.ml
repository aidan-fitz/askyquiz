open Quiz

open OUnit2
open TestUtils

let (>::) name f = ("Quiz: " ^ name) >:: f

(** [make_get_qs_test name quiz expected] constructs an OUnit test called [name]
    which asserts the equality of [get_questions quiz] with [expected]. *)
let make_get_qs_test
    (name:string)
    (quiz:Quiz.t)
    (expected:(id * string) list) =
  name >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists
                     expected (get_questions quiz))

let quiz_tests = [
  (* Fail if no questions *)
  "quiz with no questions" >:: (fun _ -> assert_equal
    (Error "JSON doesn't represent quiz.") (parse_json "quizzes/bad.quiz"));
  
  (* Testing parse_json *)
  "title" >:: (fun _ ->
    assert_equal "Which Hogwarts house are you in?" (title hp));
  "description" >:: (fun _ ->
    assert_equal "This is an example personality quiz! Source: \
                  https://www.proprofs.com/quiz-school/story.php?\
                  title=harry-potter-house-test_2" (desc hp));
  "categories" >:: (fun _ ->
    assert_equal
      ["Gryffindor"; "Hufflepuff"; "Ravenclaw"; "Slytherin"] (categories hp));
  "question ids" >:: (fun _ ->
    assert_equal ["q1"; "q2"; "q3"; "q4"; "q5"] (question_ids hp));
  "2nd question text" >:: (fun _ -> 
    assert_equal "How would you react if someone picks on you and your friend?"
    (List.nth (question_qs hp) 1));
  "3rd question text" >:: (fun _ ->
    assert_equal "What's your favorite color?" (List.nth (question_qs hp) 2));
  "1st q answer id/text" >:: (fun _ ->
    assert_equal
      [
        ("q1a1", "Save myself, of course!"); 
        ("q1a2", "Run and grab my friend who's still in the building"); 
        ("q1a3", "It depends -- if I have a way to save my friend, then yes, \
                  but if there's no way then I am above everybody else."); 
        ("q1a4", "I'll try, but I am not sure!")
      ] (get_answers (List.hd (question_ids hp)) hp));

  (* Test retrieving question text *)
  make_get_qs_test "bread questions" bread
    [
      ("qid1", "What is your favorite drink?");
      ("qid2", "How long do you sleep every night?");
      ("qid3", "What is your favorite season? ");
      ("qid4", "Who of the following inspires you the most?");
      ("qid5", "Where would you want to go for vacation?");
    ];
  make_get_qs_test "demo questions" demo
    [
      ("qid1", "What is OCaml?");
      ("qid2", "Who was the first president of America?");
    ];
]
