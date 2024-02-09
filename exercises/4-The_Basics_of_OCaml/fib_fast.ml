let rec fib_aux n =
  if n = 1 then (0, 1)
  else if n = 2 then (1, 1)
  else let (a, b) = fib_aux (n - 1)
  in (b, a + b)

let fib n =
  let (_, r) = fib_aux n
  in r