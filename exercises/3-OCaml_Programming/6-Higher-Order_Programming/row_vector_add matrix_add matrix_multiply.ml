let add_row_vector = List.map2 (+)

let add_matrices = List.map2 add_row_vector

let rec transpose = function
  | [] :: _ -> [] (* base case empty matrix *)
  | m ->
      let (r1, t) = 
        List.fold_right
          (fun row (r', t) ->
            match row with
            | [] -> failwith "invalid matrix"
            | c1 :: cs -> (c1 :: r', cs :: t)
          )
          m
          ([], [])
      in r1 :: transpose t

let multiply_matrices m1 m2 =
  let m2t = transpose m2 in 
  List.map 
    (fun row ->
      List.map
        (fun col2 ->
          List.fold_left2
            (fun acc r c -> acc + r * c )
            0
            row
            col2)
        m2t)
    m1