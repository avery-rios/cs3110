let p1 : 'a * 'b -> 'b * 'a = fun (a, b) -> (b, a)

type ('a, 'b) disj = Left of 'a | Right of 'b

let p2 : ('a, 'b) disj -> ('b, 'a) disj = function
  | Left l -> Right l
  | Right r -> Left r
