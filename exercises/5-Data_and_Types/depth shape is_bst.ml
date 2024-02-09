type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree


let rec depth = function
  | Leaf -> 0
  | Node (_, l ,r) -> 1 + max (depth l) (depth r)

let rec same_shape t1 t2 =
  match t1,t2 with
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) ->
      same_shape l1 l2 && same_shape r1 r2
  | _ -> false

type 'a tree_stat =
  | Invalid
  | Empty
  | NonEmpty of { maximum: 'a; minimum: 'a }

let rec bst_stat = function
  | Leaf -> Empty
  | Node ((v, _), l, r) -> begin
      match bst_stat l with
      | Invalid -> Invalid
      | Empty -> begin
          match bst_stat r with
          | Invalid -> Invalid
          | Empty -> NonEmpty {maximum = v; minimum = v}
          | NonEmpty {maximum = max_r; minimum = min_r} when min_r >= v ->
              NonEmpty {maximum = max_r; minimum = v}
          | NonEmpty _ -> Invalid
          end
      | NonEmpty {maximum = max_l; minimum = min_l} when max_l <= v -> begin
          match bst_stat r with
          | Invalid -> Invalid
          | Empty -> NonEmpty {maximum = v; minimum = min_l}
          | NonEmpty {maximum = max_r; minimum = min_r} when min_r >= v ->
              NonEmpty {maximum = max_r; minimum = min_l}
          | NonEmpty _ -> Invalid
          end
      | NonEmpty _ -> Invalid
  end

let is_bst  (t: ('a * 'b) tree) : bool = 
  match bst_stat t with
  | Invalid -> false
  | Empty -> true
  | NonEmpty _ -> true