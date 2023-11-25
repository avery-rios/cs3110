let rec take_aux acc n lst =
  if n = 0 then acc
  else match lst with
  | [] -> acc
  | h :: t -> take_aux (h :: acc) (n - 1) t

let take n lst = List.rev (take_aux [] n lst)
