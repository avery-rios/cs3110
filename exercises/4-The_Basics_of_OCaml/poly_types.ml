let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y

let _ = (f : bool -> bool)
let _ = (g : 'a -> bool -> 'a)
let _ = (h : bool -> 'a -> 'a -> 'a)
let _ = (i : bool -> 'a -> 'b -> 'a)