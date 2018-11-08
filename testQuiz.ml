open Quiz

open OUnit2
open TestUtils
open TestUtils.QuizUtils

let make_str_test
    (name:string)
    (output:string)
    (expected:string) =
  name >:: (fun _ -> assert_equal output expected ~printer:pp_string)

let quiz_tests = [
  (* Fail if no questions *)
  "quiz with no questions" >:: (fun _ -> assert_equal
    (Error "JSON doesn't represent quiz.") (parse_json "quizzes/bad.quiz"));
  
  (* Testing parse_json *)
  make_str_test "title" (title hp) "Which Hogwarts house are you in?";
  make_str_test "description" "This is an example personality quiz! Source: \
                  https://www.proprofs.com/quiz-school/story.php?\
                  title=harry-potter-house-test_2" (desc hp);
  make_set_test "categories" (categories hp)
    ["Gryffindor"; "Hufflepuff"; "Ravenclaw"; "Slytherin"];
  make_set_test "question ids" (question_ids hp) ["q1"; "q2"; "q3"; "q4"; "q5"];
  make_str_test "2nd question text" (List.nth (question_qs hp) 1)
    "How would you react if someone picks on you and your friend?";
  make_str_test "3rd question text" (List.nth (question_qs hp) 2)
    "What's your favorite color?";
  make_set_test "1st q's answers" (get_answers (List.hd (question_ids hp)) hp)
    [
      ("q1a1", "Save myself, of course!"); 
      ("q1a2", "Run and grab my friend who's still in the building"); 
      ("q1a3", "It depends -- if I have a way to save my friend, then yes, \
                but if there's no way then I am above everybody else."); 
      ("q1a4", "I'll try, but I am not sure!")
    ];
  
  (* Test retrieving different numbers of questions *)
  make_set_test "bread questions" (get_questions bread)
    [
      ("qid1", "What is your favorite drink?");
      ("qid2", "How long do you sleep every night?");
      ("qid3", "What is your favorite season? ");
      ("qid4", "Who of the following inspires you the most?");
      ("qid5", "Where would you want to go for vacation?");
    ];
  make_set_test "demo questions" (get_questions demo)
    [
      ("qid1", "What is OCaml?");
      ("qid2", "Who was the first president of America?");
    ];
  
  (* Test retrieving different numbers of answers *)
  make_set_test "HP - q1 answers" (get_answers "qid1" pottermore)
    [("ansid1", "Dawn"); ("ansid2", "Dusk")];
  make_set_test "HP - q7 answers" (get_answers "qid7" pottermore)
    [
      ("ansid1", "The small tortoiseshell box, embellished with gold, inside \
                  which some small creature seems to be squeaking.");
      ("ansid2", "The gleaming jet black box with a silver lock and key, \
                  marked with a mysterious rune that you know to be the mark \
                  of Merlin.");
      ("ansid3", "The ornate golden casket, standing on clawed feet, whose \
                  inscription warns that both secret knowledge and unbearable \
                  temptation lie within.");
      ("ansid4", "The small pewter box, unassuming and plain, with a scratched \
                  message upon it that reads ‘I open only for the worthy.’");
    ]
]
