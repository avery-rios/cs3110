let rec balance_rec = function
  | [] -> 0
  | h :: t -> h + balance_rec t

let balance_foldl = List.fold_left (+) 0

let balance_foldr l = List.fold_right (+) l 0