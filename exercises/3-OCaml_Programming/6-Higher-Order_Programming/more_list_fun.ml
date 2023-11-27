let longer_3 = List.filter (fun x -> String.length x > 3)

let add_one = List.map (fun x -> x +. 1.)

let concat_sep strs sep =
  match strs with
  | [] -> ""
  | h :: t -> List.fold_left (fun acc s -> acc ^ sep ^ s) h t