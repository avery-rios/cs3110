let rec concat lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ concat t