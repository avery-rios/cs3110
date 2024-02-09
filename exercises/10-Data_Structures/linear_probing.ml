module type S = sig
  type key
  type 'v t

  val create : int -> 'v t
  val insert : 'v t -> key -> 'v -> unit
  val remove : 'v t -> key -> unit
  val find_opt : 'v t -> key -> 'v option
  val find : 'v t -> key -> 'v
  val bindings : 'v t -> (key * 'v) list
  val of_list : (key * 'v) list -> 'v t
end

module MakeImpl (K : Hashtbl.HashedType) = struct
  type key = K.t
  type 'v entry = Present of int * key * 'v | Deleted | Vacant

  type 'v t = {
    mutable size : int;
    mutable deleted : int;
    mutable buckets : 'v entry array;
  }

  let create cap = { size = 0; deleted = 0; buckets = Array.make cap Vacant }

  (** [def_index cap h] is index of [h] in [cap] buckets when no collision is found *)
  let def_index cap h = h mod cap

  (** [next_index cap i] is next index of bucket length [cap] *)
  let next_index cap h = if h = cap then 0 else h + 1

  let index_opt tab h k =
    let cap = Array.length tab.buckets in
    let rec idx_i p =
      match tab.buckets.(p) with
      | Present (h1, k1, v) ->
          if h1 = h && K.equal k1 k then Some (p, v)
          else idx_i (next_index cap p)
      | Deleted -> idx_i (next_index cap p)
      | Vacant -> None
    in
    idx_i (def_index cap h)

  let insert_no_rehash tab h k v =
    let cap = Array.length tab.buckets in
    let new_ent = Present (h, k, v) in
    let rec ins_i p =
      match tab.buckets.(p) with
      | Present (h1, k1, _) ->
          if h1 = h && K.equal k1 k then tab.buckets.(p) <- new_ent
          else ins_i (next_index cap p)
      | Deleted ->
          tab.buckets.(p) <- new_ent;
          tab.deleted <- tab.deleted - 1
      | Vacant -> tab.buckets.(p) <- new_ent
    in
    ins_i (def_index cap h)

  let rehash tab size =
    let old_br = tab.buckets in
    tab.buckets <- Array.make size Vacant;
    tab.deleted <- 0;
    Array.iter
      (function
        | Present (h, k, v) -> insert_no_rehash tab h k v
        | Deleted -> ()
        | Vacant -> ())
      old_br

  (** [load_factor_inc tab] is table [tab] load factor for double size *)
  let load_factor_inc tab =
    float_of_int (tab.size + tab.deleted)
    /. float_of_int (Array.length tab.buckets)

  let insert tab k v =
    insert_no_rehash tab (K.hash k) k v;
    tab.size <- tab.size + 1;
    if load_factor_inc tab > 0.5 then rehash tab (Array.length tab.buckets * 2)
    else ()

  let load_factor_dec tab =
    float_of_int tab.size /. float_of_int (Array.length tab.buckets)

  let remove tab k =
    match index_opt tab (K.hash k) k with
    | Some (idx, _) ->
        tab.buckets.(idx) <- Deleted;
        tab.deleted <- tab.deleted + 1;
        tab.size <- tab.size - 1;
        if load_factor_dec tab < 1. /. 8. then
          rehash tab (Array.length tab.buckets / 2)
        else ()
    | None -> ()

  let find_opt tab k = index_opt tab (K.hash k) k |> Option.map snd
  let find tab k = Option.get (find_opt tab k)

  let bindings tab =
    Array.fold_left
      (fun i e ->
        match e with
        | Present (_, k, v) -> (k, v) :: i
        | Deleted -> i
        | Vacant -> i)
      [] tab.buckets

  let of_list lst =
    let size = List.length lst in
    let tab = create (size * 2) in
    List.iter (fun (k, v) -> insert_no_rehash tab (K.hash k) k v) lst;
    tab.size <- size;
    tab
end

module Make (K : Hashtbl.HashedType) : S with type key = K.t = MakeImpl (K)
