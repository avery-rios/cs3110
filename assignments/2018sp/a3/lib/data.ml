module type Formattable = sig
  type t

  val format : Format.formatter -> t -> unit
end

module type Comparable = sig
  type t

  val compare : t -> t -> [ `EQ | `GT | `LT ]

  include Formattable with type t := t
end

(* type ('k, 'v) avl =
   | Leaf
   | Node of ('k * 'v) * ('k, 'v) avl * ('k, 'v) avl * int *)

module type Dictionary = sig
  module Key : Comparable
  module Value : Formattable

  type key = Key.t
  type value = Value.t
  type t

  val rep_ok : t -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : key -> value -> t -> t
  val member : key -> t -> bool
  val find : key -> t -> value option
  val remove : key -> t -> t
  val choose : t -> (key * value) option
  val fold : (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> (key * value) list

  (* val expose_tree : t -> (key, value) avl *)
  val format : Format.formatter -> t -> unit
end

module type DictionaryMaker = functor (K : Comparable) (V : Formattable) ->
  Dictionary with module Key = K and module Value = V

module MakeListDictionary (K : Comparable) (V : Formattable) = struct
  module Key = K
  module Value = V

  type key = K.t
  type value = V.t

  (* AF: [[(k1,v1); (k2,v2);..;(kn,vn)]] is the map {k1:v1, k2:v2,k3:v3,..,kn:vn}
   * RI: List contains no duplicate key, and in least to greatest order *)
  type t = (key * value) list

  let rec check_rep lst = function
    | [] -> ()
    | (k, _) :: t -> (
        match K.compare lst k with
        | `LT -> check_rep k t
        | `EQ -> failwith "list contains duplicate key"
        | `GT -> failwith "list is not in order")

  let rep_ok = function
    | [] -> []
    | (k0, _) :: t as d ->
        check_rep k0 t;
        d

  let empty = []
  let is_empty = function [] -> true | _ :: _ -> false
  let size = List.length

  let rec insert k v d =
    match d with
    | [] -> [ (k, v) ]
    | ((k0, _) as b) :: t -> (
        match K.compare k0 k with
        | `LT -> b :: insert k v t
        | `EQ -> (k, v) :: t
        | `GT -> (k, v) :: d)

  let remove = List.remove_assoc

  let rec find k = function
    | [] -> None
    | (k0, v) :: t -> (
        match K.compare k0 k with
        | `LT -> find k t
        | `EQ -> Some v
        | `GT -> None)

  let member k d = find k d |> Option.is_some
  let choose = function [] -> None | b :: _ -> Some b
  let to_list d = d
  let fold f = List.fold_left (fun acc (k, v) -> f k v acc)
  (* let expose_tree _ = failwith "not an AVL tree" *)

  let format fmt d =
    let open Format in
    pp_print_char fmt '{';
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
      (fun fmt (k, v) ->
        pp_open_box fmt 0;
        K.format fmt k;
        pp_print_string fmt " : ";
        V.format fmt v;
        pp_close_box fmt ())
      fmt d;
    pp_print_char fmt '}'
end

module MakeTreeDictionary =
functor
  (K : Comparable)
  (V : Formattable)
  ->
  struct
    module Key = K
    module Value = V

    type key = K.t
    type value = V.t
    type branch
    type leaf
    type 'pos color = R : branch color | B : 'pos color | BB : 'pos color

    (** RI:
        - Leaf node is black
        - Black depth on every path is equal
        - If a node is red, then root of left and right subtree is black
        - All keys in left subtree is less then root, and all keys in right
          subtree is greater than root *)
    type t = Leaf of leaf color | Branch of branch color * key * value * t * t

    (** [color t] is color of node [t] *)
    let color = function Leaf _ -> B | Branch (c, _, _, _, _) -> c

    type rep_error = LocalInv | GlobalInv | BSTInv | DoubleBlack

    exception InvalidRep of rep_error

    let invalid_bb () = raise (InvalidRep DoubleBlack)

    (** [check_bsr_br k l r] returns min and max element of tree
        [Branch(_,k, _, l, r)]. 
        Raises: [InvalidRep BSTInv] is bst invariant is violated  *)
    let rec check_bst_br k l r =
      let max_k =
        match r with
        | Leaf _ -> k
        | Branch (_, kr, _, rl, rr) ->
            let min_r, max_r = check_bst_br kr rl rr in
            if K.compare min_r k <> `GT then raise (InvalidRep BSTInv)
            else max_r
      in
      let min_k =
        match l with
        | Leaf _ -> k
        | Branch (_, kl, _, ll, lr) ->
            let min_l, max_l = check_bst_br kl ll lr in
            if K.compare max_l k <> `LT then raise (InvalidRep BSTInv)
            else min_l
      in
      (min_k, max_k)

    (** [check_bst k l r] checks BST invariant of root key [k] and left subtree [l]
        and right subtree [t] *)
    let check_bst = function
      | Leaf _ -> ()
      | Branch (_, k, _, l, r) -> ignore (check_bst_br k l r)

    (** [check_rep t] returns black depth of tree [t]. 
        Raises: [InvalidRep] when invariant is violated *)
    let rec check_rep = function
      | Leaf c -> (
          match c with B -> 0 | BB -> raise (InvalidRep DoubleBlack))
      | Branch (c, _, _, l, r) ->
          let dl = check_rep l in
          let dr = check_rep r in
          if dl <> dr then raise (InvalidRep GlobalInv);
          (match c with
          | B -> ()
          | R ->
              if color l <> B || color r <> B then raise (InvalidRep LocalInv)
          | BB -> raise (InvalidRep DoubleBlack));
          dl

    let rep_ok t =
      let _ = check_rep t in
      let _ = check_bst t in
      t

    let empty = Leaf B
    let is_empty = function Leaf _ -> true | Branch _ -> false

    let rec size = function
      | Leaf _ -> 0
      | Branch (_, _, _, l, r) -> 1 + size l + size r

    (** [balance_b k v l r] fix local violation in subtree [l] and [r] at
        black root [k,v] *)
    let balance_b k v l r =
      match ((l, r), k, v) with
      | (Branch (R, ky, vy, Branch (R, kx, vx, a, b), c), d), kz, vz
      | (Branch (R, kx, vx, a, Branch (R, ky, vy, b, c)), d), kz, vz
      | (a, Branch (R, kz, vz, Branch (R, ky, vy, b, c), d)), kx, vx
      | (a, Branch (R, ky, vy, b, Branch (R, kz, vz, c, d))), kx, vx ->
          Branch (R, ky, vy, Branch (B, kx, vx, a, b), Branch (B, kz, vz, c, d))
      | _ -> Branch (B, k, v, l, r)

    let may_balance_ins modified c k v l r =
      if modified && c = B then balance_b k v l r else Branch (c, k, v, l, r)

    (** [insert_f k v t] is the tree inserted with k,v and whether new node is
        added. *)
    let rec insert_f k v = function
      | Leaf _ -> (Branch (R, k, v, Leaf B, Leaf B), true)
      | Branch (c, k0, v0, l, r) -> (
          match K.compare k k0 with
          | `LT ->
              let nl, added = insert_f k v l in
              (may_balance_ins added c k0 v0 nl r, added)
          | `EQ -> (Branch (c, k, v, l, r), false)
          | `GT ->
              let nr, added = insert_f k v r in
              (may_balance_ins added c k0 v0 l nr, added))

    (** [blacken t] is tree [t] with root color black when necessary *)
    let blacken = function
      | Branch (R, k, v, (Branch (R, _, _, _, _) as l), r)
      | Branch (R, k, v, l, (Branch (R, _, _, _, _) as r)) ->
          Branch (B, k, v, l, r)
      | t -> t

    let insert k v t = insert_f k v t |> fst |> blacken

    (** [balance_bb k v l r] is balance tree [Branch(BB, k, v, l, r)] and
        contains whether double black after balance *)
    let balance_bb k v l r =
      match (l, r, k, v) with
      | Branch (R, kx, vx, a, Branch (R, ky, vy, b, c)), d, kz, vz
      | a, Branch (R, kz, vz, Branch (R, ky, vy, b, c), d), kx, vx ->
          ( Branch
              (B, ky, vy, Branch (B, kx, vx, a, b), Branch (B, kz, vz, c, d)),
            false )
      | _ -> (Branch (BB, k, v, l, r), true)

    let rotate c k v l r =
      match (c, k, v, l, r) with
      | R, ky, vy, Branch (BB, kx, vx, a, b), Branch (B, kz, vz, c, d) ->
          ( balance_b kz vz (Branch (R, ky, vy, Branch (B, kx, vx, a, b), c)) d,
            false )
      | R, ky, vy, Leaf BB, Branch (B, kz, vz, c, d) ->
          (balance_b kz vz (Branch (R, ky, vy, Leaf B, c)) d, false)
      | R, ky, vy, Branch (B, kx, vx, a, b), Branch (BB, kz, vz, c, d) ->
          ( balance_b kx vx a (Branch (R, ky, vy, b, Branch (B, kz, vz, c, d))),
            false )
      | R, ky, vy, Branch (B, kx, vx, a, b), Leaf BB ->
          (balance_b kx vx a (Branch (R, ky, vy, b, Leaf B)), false)
      | B, ky, vy, Branch (BB, kx, vx, a, b), Branch (B, kz, vz, c, d) ->
          balance_bb kz vz (Branch (R, ky, vy, Branch (B, kx, vx, a, b), c)) d
      | B, ky, vy, Leaf BB, Branch (B, kz, vz, c, d) ->
          balance_bb kz vz (Branch (R, ky, vy, Leaf B, c)) d
      | B, ky, vy, Branch (B, kx, vx, a, b), Branch (BB, kz, vz, c, d) ->
          balance_bb kx vx a (Branch (R, ky, vy, b, Branch (B, kz, vz, c, d)))
      | B, ky, vy, Branch (B, kx, vx, a, b), Leaf BB ->
          balance_bb kx vx a (Branch (R, ky, vy, b, Leaf B))
      | ( B,
          kx,
          vx,
          Branch (BB, kw, vw, a, b),
          Branch (R, kz, vz, Branch (B, ky, vy, c, d), e) ) ->
          ( Branch
              ( B,
                kz,
                vz,
                balance_b ky vy
                  (Branch (R, kx, vx, Branch (B, kw, vw, a, b), c))
                  d,
                e ),
            false )
      | B, kx, vx, Leaf BB, Branch (R, kz, vz, Branch (B, ky, vy, c, d), e) ->
          ( Branch
              (B, kz, vz, balance_b ky vy (Branch (R, kx, vx, Leaf B, c)) d, e),
            false )
      | ( B,
          ky,
          vy,
          Branch (R, kw, vw, a, Branch (B, kx, vx, b, c)),
          Branch (BB, kz, vz, d, e) ) ->
          ( Branch
              ( B,
                kw,
                vw,
                a,
                balance_b kx vx b
                  (Branch (R, ky, vy, c, Branch (B, kz, vz, d, e))) ),
            false )
      | B, ky, vy, Branch (R, kw, vw, a, Branch (B, kx, vx, b, c)), Leaf BB ->
          ( Branch
              (B, kw, vw, a, balance_b kx vx b (Branch (R, ky, vy, c, Leaf B))),
            false )
      | _ -> (Branch (c, k, v, l, r), true)

    (** [may_rotate_del has_bb c k v l r] is the tree [Branch(c,k,v,l,r)] rotated
        when [has_bb = true], unchanged if [has_bb = false] *)
    let may_rotate_del has_bb c k v l r =
      if has_bb then rotate c k v l r else (Branch (c, k, v, l, r), false)

    (** [delete_min_single c r] is tree [Branch(c,_, _, Leaf _, r)] with
        min removed *)
    let delete_min_single c r =
      match (c, r) with
      | R, _ -> (r, false)
      | B, Leaf _ -> (Leaf BB, true)
      | B, Branch (rc, kr, vr, rl, rr) -> (
          match rc with
          | R -> (Branch (B, kr, vr, rl, rr), false)
          | B -> balance_bb kr vr rl rr
          | BB -> invalid_bb ())
      | BB, _ -> invalid_bb ()

    (** [delete_min c k v l r = (m, (t1, has_bb))]. [m] is the min element of 
        tree [Branch(c, k, v, l, r)], [t1] is tree with [m] deleted. 
        [has_bb = true] when there is double black node  *)
    let rec delete_min c k v l r =
      match l with
      | Leaf _ -> ((k, v), delete_min_single c r)
      | Branch (lc, kl, vl, ll, rl) ->
          let ret, (nl, has_bb) = delete_min lc kl vl ll rl in
          (ret, may_rotate_del has_bb c k v nl r)

    let remove_node_single c l =
      match (c, l) with
      | R, l -> (l, false)
      | B, Leaf _ -> (Leaf BB, true)
      | B, Branch (_, kl, vl, ll, rl) ->
          (* Branch must be Branch(R, _, _, Leaf _, Leaf _) *)
          (Branch (B, kl, vl, ll, rl), false)
      | BB, _ -> invalid_bb ()

    let remove_node c l r =
      match r with
      | Leaf _ -> remove_node_single c l
      | Branch (cr, kr, vr, rl, rr) ->
          let (km, vm), (nr, _) = delete_min cr kr vr rl rr in
          rotate c km vm l nr

    let rec remove_f k = function
      | Leaf _ as t -> (t, false)
      | Branch (c, k0, v, l, r) -> (
          match K.compare k k0 with
          | `LT ->
              let nl, has_bb = remove_f k l in
              may_rotate_del has_bb c k0 v nl r
          | `EQ -> remove_node c l r
          | `GT ->
              let nr, has_bb = remove_f k r in
              may_rotate_del has_bb c k0 v l nr)

    let remove k = function
      | Leaf _ as n -> n (* empty tree, no nothing *)
      | Branch
          ( B,
            k0,
            v,
            (Branch (B, _, _, _, _) as l),
            (Branch (B, _, _, _, _) as r) ) ->
          (* redden root *)
          remove_f k (Branch (R, k0, v, l, r)) |> fst
      | t -> remove_f k t |> fst

    let rec find k = function
      | Leaf _ -> None
      | Branch (_, k0, v, l, r) -> (
          match Key.compare k k0 with
          | `LT -> find k l
          | `EQ -> Some v
          | `GT -> find k r)

    let member k d = find k d |> Option.is_some

    let choose = function
      | Leaf _ -> None
      | Branch (_, k, v, _, _) -> Some (k, v)

    let rec fold f init = function
      | Leaf _ -> init
      | Branch (_, k, v, l, r) -> fold f (f k v (fold f init l)) r

    let rec to_list_acc acc = function
      | Leaf _ -> acc
      | Branch (_, k, v, l, r) -> to_list_acc ((k, v) :: to_list_acc acc r) l

    let to_list = to_list_acc []

    let rec format fmt d =
      let open Format in
      match d with
      | Leaf B -> pp_print_string fmt "Leaf(B)"
      | Leaf BB -> pp_print_string fmt "Leaf(BB)"
      | Branch (c, k, v, l, r) ->
          pp_print_string fmt "Branch(";
          pp_print_string fmt (match c with R -> "R" | B -> "B" | BB -> "BB");
          pp_print_char fmt ',';
          pp_print_space fmt ();

          K.format fmt k;
          pp_print_space fmt ();
          pp_print_string fmt "=>";
          pp_print_space fmt ();
          V.format fmt v;
          pp_print_char fmt ',';
          pp_print_space fmt ();

          format fmt l;
          pp_print_char fmt ',';
          pp_print_space fmt ();

          format fmt r;
          pp_print_char fmt ')'
  end

module type Set = sig
  module Elt : Comparable

  type elt = Elt.t
  type t

  val rep_ok : t -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

module MakeSetOfDictionary (C : Comparable) (DM : DictionaryMaker) = struct
  module Elt = C

  module Dict =
    DM
      (C)
      (struct
        type t = unit

        let format _ _ = ()
      end)

  type elt = Elt.t
  type t = Dict.t

  let rep_ok = Dict.rep_ok
  let empty = Dict.empty
  let is_empty = Dict.is_empty
  let size = Dict.size
  let insert x = Dict.insert x ()
  let member = Dict.member
  let remove = Dict.remove
  let choose s = Dict.choose s |> Option.map fst
  let fold f = Dict.fold (fun k _ acc -> f k acc)
  let union s1 s2 = fold insert s1 s2

  let intersect s1 s2 =
    fold (fun v a -> if member v s1 then insert v a else a) empty s2

  let difference s1 s2 = fold remove s1 s2
  let to_list s = Dict.to_list s |> List.map fst

  let format fmt d =
    let open Format in
    pp_print_char fmt '{';
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
      (fun fmt v ->
        pp_open_box fmt 0;
        C.format fmt v;
        pp_close_box fmt ())
      fmt (to_list d);
    pp_print_char fmt '}'
end
