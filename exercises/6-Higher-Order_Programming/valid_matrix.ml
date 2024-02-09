let is_valid_matrix mat =
  match mat with
  | [] -> false
  | h :: t ->
      let col = List.length h in
      col > 0 && List.for_all (fun r -> List.length r = col) t