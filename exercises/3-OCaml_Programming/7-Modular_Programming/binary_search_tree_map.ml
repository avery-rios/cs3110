module type Map = sig
  type key
  type 'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding_opt : 'a t -> (key * 'a) option
  val choose_opt : 'a t -> (key * 'a) option
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
end

module BstMap (Ord: Stdlib.Map.OrderedType) : Map with type key = Ord.t = struct
  type key = Ord.t

  type 'a t = 
      Leaf
    | Node of key * 'a * 'a t * 'a t

  let empty = Leaf
  
  let rec add k v = function
    | Leaf -> Node (k, v, Leaf, Leaf)
    | Node(k1, v1, l, r) ->
        let cm = Ord.compare k k1 in
        if cm < 0 then Node(k1, v1, add k v l, r)
        else if cm > 0 then Node(k1, v1, l, add k v r)
        else Node(k, v1, l, r)
  
  let rec split_min = function
    | Leaf -> None
    | Node(k, v, l, r) -> begin
        match split_min l with
        | None -> Some((k, v), r)
        | Some(kv, l1) -> Some(kv, Node(k, v, l1, r))
    end
  
  let merge_lr l r =
    match split_min r with
    | None -> l
    | Some((kr, vr), r1) -> Node(kr, vr, l, r1)

  let rec update k f = function
    | Leaf -> begin
        match f None with
        | Some(v) -> Node(k, v, Leaf, Leaf)
        | None -> Leaf
      end
    | Node(k1, v1, l, r) ->
        let cm = Ord.compare k k1 in
        if cm < 0 then Node(k1, v1, update k f l, r)
        else if cm > 0 then Node(k1, v1, l, update k f r)
        else begin
          match f (Some v1) with
          | None -> merge_lr l r
          | Some(v2) -> Node(k1, v2, l, r)
        end
  
  let singleton k v = Node(k, v, Leaf, Leaf)
  
  let rec remove k = function
    | Leaf -> Leaf
    | Node(k1, v, l, r) ->
        let cm = Ord.compare k k1 in
        if cm < 0 then Node(k1, v, remove k l, r)
        else if cm > 0 then Node(k1, v, l, remove k r)
        else merge_lr l r
  
  let rec cardinal = function
    | Leaf -> 0
    | Node(_, _, l, r) -> 1 + cardinal l + cardinal r
  
  let rec bindings_aux acc = function
    | Leaf -> acc
    | Node(k, v, l, r) -> bindings_aux ((k, v) :: bindings_aux acc r) l
  
  let bindings t = bindings_aux [] t

  let rec min_binding_opt = function
    | Leaf -> None
    | Node(k, v, l, _) -> begin
        match min_binding_opt l with
        | None -> Some((k, v))
        | Some(kv) -> Some(kv)
      end
  
  let rec max_binding_opt = function
    | Leaf -> None
    | Node(k, v, _, r) -> begin
        match max_binding_opt r with
        | None -> Some((k, v))
        | Some(kv) -> Some(kv)
      end
  
  let rec choose_opt = function
    | Leaf -> None
    | Node(k, v, _, _) -> Some((k, v))

  let rec mem k = function
    | Leaf -> false
    | Node(k1, _, l, r) ->
        let cm = Ord.compare k k1 in
        if cm < 0 then mem k l
        else if cm > 0 then mem k r
        else true
  
  let rec find_opt k = function
    | Leaf -> None
    | Node(k1, v, l, r) ->
        let cm = Ord.compare k k1 in
        if cm < 0 then find_opt k l
        else if cm > 0 then find_opt k r
        else Some v
  
  let find k tree = 
    match find_opt k tree with
    | Some(v) -> v
    | None -> raise Not_found

  let rec find_first_opt f = function
    | Leaf -> None
    | Node(k, v, l, r) ->
        if f k then begin
          match find_first_opt f l with
          | None -> Some(k, v)
          | Some(kv) -> Some(kv)
        end else find_first_opt f r
  
  let rec fold f t acc = 
    match t with
    | Leaf -> acc
    | Node(k, v, l, r) ->
        fold f l (f k v (fold f r acc))

  let rec map f = function
    | Leaf -> Leaf
    | Node(k, v, l, r) -> Node(k, f v, map f l, map f r)
  
  let rec filter f = function
    | Leaf -> Leaf
    | Node(k, v, l, r) -> 
        let fl = filter f l in
        let fr = filter f r in
        if f v then Node(k, v, fl, fr)
        else merge_lr fl fr
  
  let rec filter_map f = function
    | Leaf -> Leaf
    | Node(k, v, l, r) ->
        let fl = filter_map f l in
        let fr = filter_map f r in begin
          match f k v with
          | None -> merge_lr fl fr
          | Some(v1) -> Node(k, v1, fl, fr)
        end
end