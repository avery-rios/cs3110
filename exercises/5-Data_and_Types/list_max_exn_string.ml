let rec list_max_aux def = function
  | [] -> def
  | h :: t ->
      if h > def then list_max_aux h t
      else list_max_aux def t

let list_max_string = function
  | [] -> "empty"
  | h :: t -> string_of_int (list_max_aux h t)