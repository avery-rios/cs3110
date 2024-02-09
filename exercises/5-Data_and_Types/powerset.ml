let rec cons_opt acc e lst =
  match lst with
  | [] -> acc
  | h :: t -> cons_opt ((e :: h) :: h :: acc) e t

let rec powerset lst =
  match lst with
  | [] -> [[]]
  | h :: t -> cons_opt [] h (powerset t)