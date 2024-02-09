let sum_cube_odd n =
  List.init (n + 1) (fun x -> x)
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.fold_left (fun acc x -> acc + x * x * x) 0