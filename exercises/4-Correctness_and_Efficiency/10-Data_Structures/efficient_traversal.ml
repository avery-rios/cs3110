type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec preorder_acc acc = function
  | Leaf -> acc
  | Node (l, v, r) -> v :: preorder_acc (preorder_acc acc r) l

let preorder = preorder_acc []

let rec inorder_acc acc = function
  | Leaf -> acc
  | Node (l, v, r) -> inorder_acc (v :: inorder_acc acc r) l

let inorder = inorder_acc []

let rec postorder_acc acc = function
  | Leaf -> acc
  | Node (l, v, r) -> postorder_acc (postorder_acc (v :: acc) r) l

let rec postorder = postorder_acc []

let t =
  Node
    ( Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)),
      4,
      Node (Node (Leaf, 5, Leaf), 6, Node (Leaf, 7, Leaf)) )

(*
t is
4
/
 \
2
 6
/ \
 / \
1
 3 5
 7
*)
let () = assert (preorder t = [ 4; 2; 1; 3; 6; 5; 7 ])
let () = assert (inorder t = [ 1; 2; 3; 4; 5; 6; 7 ])
let () = assert (postorder t = [ 1; 3; 2; 5; 7; 6; 4 ])
