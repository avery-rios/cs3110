let rec list_max_aux def = function
  | [] -> def
  | h :: t -> 
    if h > def then list_max_aux h t
    else list_max_aux def t

let list_max = function
  | [] -> failwith "list_max"
  | h :: t -> list_max_aux h t