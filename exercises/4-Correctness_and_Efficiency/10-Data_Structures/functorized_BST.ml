module type KeyType = sig
  type t

  val compare : t -> t -> int
end

module Make (K : KeyType) = struct
  type elt = K.t
  type t = Branch of elt * t * t | Leaf

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Branch (k, l, r) ->
        let cmp = K.compare x k in
        if cmp < 0 then mem x l else if cmp > 0 then mem x r else true

  let rec insert x = function
    | Leaf -> Branch (x, Leaf, Leaf)
    | Branch (k, l, r) as n ->
        let cmp = K.compare x k in
        if cmp < 0 then Branch (k, insert x l, r)
        else if cmp > 0 then Branch (k, l, insert x r)
        else n
end
