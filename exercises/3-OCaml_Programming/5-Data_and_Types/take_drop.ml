let rec take n lst =
  if n = 0 then []
  else match lst with
  | [] -> []
  | h :: t -> h :: take (n - 1) t

let rec drop n lst =
  if n = 0 then lst
  else match lst with
  | [] -> []
  | _ :: t -> drop (n - 1) t