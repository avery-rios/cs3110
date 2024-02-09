let is_bigred lst = 
  match lst with
  | "bigred" :: _ -> true
  | _ -> false

let two_or_four lst =
  match lst with
  | [_;_] | [_;_;_;_] -> true
  | _ -> false

let first_equal lst =
  match lst with
  | h1 :: h2 :: _ -> h1 = h2
  | _ -> false