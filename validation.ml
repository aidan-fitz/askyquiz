let normalize str =
  str |> String.lowercase_ascii
      |> Str.split (Str.regexp "[ \t\n\r\012\\.,;:\\?!\\(\\)\"]+")
      |> String.concat " "
      |> String.trim

let get_aid ltr options =
  match List.assoc_opt (String.uppercase_ascii ltr) options with
  | None -> Error "could not match"
  | Some (id, _) -> Ok id

let substr needle haystack =
  Str.string_match (Str.regexp_string needle) haystack 0

let match_answer_text input options =
  let input_norm = normalize input in
  options |> List.map (fun (_, (id, text)) -> (id, normalize text))
          |> List.filter (fun (id, text) -> substr input_norm text)
          |> List.map fst

let user_answer input options =
  match String.length input with
  | 0 -> Error "empty response"
  | 1 -> get_aid input options
  | _ ->
    match match_answer_text input options with
    | [] -> Error "could not match"
    | [id] -> Ok id
    | _ -> Error "matched multiple"
