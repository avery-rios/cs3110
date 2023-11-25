let last lst = List.nth lst (List.length lst - 1)
let any_zeroes lst = List.exists (fun e -> e = 0) lst