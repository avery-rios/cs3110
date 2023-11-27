let rec repeat f n x =
  if n = 0 then x
  else repeat f (n - 1) (f x)