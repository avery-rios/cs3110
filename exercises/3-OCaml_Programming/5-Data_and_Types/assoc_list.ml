let insert k v lst = (k, v) :: lst

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let assoc_list =
  [] 
  |> insert 1 "one" 
  |> insert 2 "two" 
  |> insert 3 "three"

let v2 = lookup 2 assoc_list

let v4 = lookup 4 assoc_list