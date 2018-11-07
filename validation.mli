(** [normalize str] is [str] normalized: the string is converted to lowercase,
    then any span of whitespace ([ \t\n\r\012]) and punctuation ([.,;:?!()""])
    is converted to a single space, and finally the string is trimmed. *)
val normalize : string -> string

(** [substr needle haystack] is [true] if [needle] is a substring of [haystack]
    or [false] otherwise. *)
val substr : string -> string -> bool

(** [get_aid ltr mapping] returns [Some id] if [id] is associated with the 
    letter [ltr] in mapping or [None] if no such id exists. *)
val get_aid : string -> (string * (Quiz.id * string)) list -> Quiz.id option

(** [match_answer_text input mapping] returns a list of answer ids that match
    the user input; that is, all answer ids for which [normalize input] is a
    substring of the normalized answer text. *)
val match_answer_text : string -> (string * (Quiz.id * string)) list -> Quiz.id list

(** [user_answer input options] returns:
    - [Ok id] if the user input matches one answer choice, identified by [id].
    - [Error "empty response"] if the user input is empty.
    - [Error "could not match"] if the user input does not match any answer 
      choice.
    - [Error "matched multiple"] if the user input matches multiple answer
      choices. *)
val user_answer : string -> (string * (Quiz.id * string)) list -> (Quiz.id, string) result
