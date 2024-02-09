type date = int * int * int

let is_before (y1,m1,d1) (y2,m2,d2) =
  if y1 < y2 then true
  else y1 = y2 && (
    if m1 < m2 then true
    else m1 = m2 && d1 < d2
  )

let rec earliest_aux def = function
  | [] -> def
  | h :: t -> 
    if is_before h def then earliest_aux h t
    else earliest_aux def t

let earliest lst =
  match lst with
  | [] -> None
  | h :: t -> Some (earliest_aux h t)