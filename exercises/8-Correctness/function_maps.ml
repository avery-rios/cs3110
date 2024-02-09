module type MapT = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t
  (** [empty] is empty map *)

  val mem : 'k -> ('k, 'v) t -> bool
  (** [mem k t = true] when k is a member of t  *)

  val find : 'k -> ('k, 'v) t -> 'v option
  (** [find k t] returns [Some v] when (k,v) is in t, [None] otherwise *)

  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [add k v t] returns a map with k maps to v. *)

  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  (** [remove k t] removes k from map t. *)
end

module FunMap : MapT = struct
  type ('k, 'v) t = 'k -> 'v option

  let empty _ = None
  let mem k f = f k |> Option.is_some
  let find k f = f k
  let add k v f k1 = if k1 = k then Some v else f k1
  let remove k f k1 = if k1 = k then None else f k1
end
