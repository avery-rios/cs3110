type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec nested n = if n = 0 then Leaf else Node (Leaf, 0, nested (n - 1))
let _ = assert (Hashtbl.hash (nested 10) = Hashtbl.hash (nested 11))
