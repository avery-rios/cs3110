let rec product = function
  | [] -> 1
  | h :: t -> h * product t