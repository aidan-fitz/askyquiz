let normalize str =
  str |> String.lowercase_ascii
      |> Str.split (Str.regexp "[ \t\n\r\012\\.,;:\\?!\\(\\)\"]+")
      |> String.concat " "
      |> String.trim

let match_answer_letter ltr options =
  match List.assoc_opt (String.uppercase_ascii ltr) options with
  | None -> Error "could not match"
  | Some (id, _) -> Ok id

let substr needle haystack =
  Str.string_match (Str.regexp_string needle) haystack 0

(** [edit_dist str1 str2] is the edit distance between [str1] and [str2],
    assuming that one of them is a substring of the other. *)
let edit_dist str1 str2 =
  abs ((String.length str1) - (String.length str2))

(** [all_shortest_dist lst] is the list of ids that all have the shortest edit
    distance, where [lst] is a list of [(id, dist)] pairs. *)
let all_shortest_dist lst =
  let sorted = List.fast_sort 
                (fun (_, dist1) (_, dist2) -> compare dist1 dist2) lst in
  let rec get_all_min = function
    | [] -> []
    | item :: [] -> [item]
    | (id1, n1) :: (_, n2) :: _ when n1 <> n2 -> [(id1, n1)]
    | (id1, n1) :: (id2, n2) :: tail ->
      (id1, n1) :: get_all_min ((id2, n2) :: tail)
  in
  sorted |> get_all_min |> List.map fst

let match_answer_text input options =
  let input_norm = normalize input in
  options |> List.map (fun (_, (id, text)) -> (id, normalize text))
          |> List.filter (fun (id, text) -> substr input_norm text)
          |> List.map (fun (id, text) -> (id, edit_dist input_norm text))
          |> all_shortest_dist

let user_answer input options =
  match String.length input with
  | 0 -> Error "empty response"
  | 1 -> match_answer_letter input options
  | _ ->
    match match_answer_text input options with
    | [] -> Error "could not match"
    | [id] -> Ok id
    | _ -> Error "matched multiple"
