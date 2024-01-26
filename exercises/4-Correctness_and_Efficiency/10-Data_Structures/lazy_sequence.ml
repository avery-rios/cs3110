type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

let rec map f (Cons (h, t)) = Cons (f h, lazy (map f (Lazy.force t)))

let rec filter f (Cons (h, t)) =
  if f h then Cons (h, lazy (filter f (Lazy.force t)))
  else filter f (Lazy.force t)
