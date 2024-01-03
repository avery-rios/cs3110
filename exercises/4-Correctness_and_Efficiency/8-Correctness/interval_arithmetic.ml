module type IntervalT = sig
  type t
  (** [t] is type of interval *)

  val empty : t
  (** [empty] is the empty interval *)

  val is_empty : t -> bool
  (** [is_empty i = true] if and only if i is an empty interval*)

  val from_bounds : int -> int -> t
  (** [from_bounds l r] is interval l..r.
      Requires: l <= r *)

  val from_midpoint : int -> int -> t
  (** [from_midpoint m r] is interval m-r .. m+r *)

  val lower : t -> int option
  (** [low i] is lower bound of interval i, return [None] if interval is empty *)

  val upper : t -> int option
  (** [upper i] is upper bound of interval i, returns [None] if interval is empty  *)

  val width : t -> int option
  (** [width i] is the width of interval i, returne [None] if interval is empty *)

  val mem : int -> t -> bool
  val intersect : t -> t -> t

  val union : t -> t -> t
  (** [union i1 i2] is union interval of i1 and i2.*)

  val add : t -> t -> t
  val minus : t -> t -> t
  val mul : t -> t -> t

  val div : t -> t -> t
  (** [div i1 i2] is interval i1 / i2.
      Requires: 0 not in i2 *)

  val to_string : t -> string
  val format : Format.formatter -> t -> unit
end

module IntervalImpl : IntervalT = struct
  type t = (int * int) option
  (** [Some (l,h)] represents interval l..h. [None] represents empty interval
      RI: [Some(l, h)] requires l <= h *)

  let rep_ok i =
    match i with
    | None -> i
    | Some (l, r) -> if l <= r then i else failwith "Invalid interval"

  let empty = None
  let is_empty = Option.is_none
  let from_bounds l r = Some (l, r)
  let from_midpoint m r = Some (m - r, m + r)
  let lower = Option.map fst
  let upper = Option.map snd
  let width = Option.map (fun (l, r) -> r - l)
  let mem i = function None -> false | Some (l, r) -> l <= i && i <= r
  let is_intersect (l1, r1) (l2, r2) = l2 <= r1 || l1 <= r2
  let ( let* ) = Option.bind

  let liftA2 f i1 i2 =
    match (i1, i2) with
    | Some v1, Some v2 -> Some (f v1 v2)
    | None, _ -> None
    | _, None -> None

  let minmax4 a b c d =
    let l1, r1 = if a < b then (a, b) else (b, a) in
    let l2, r2 = if c < d then (c, d) else (d, c) in
    (min l1 l2, max r1 r2)

  let intersect i1 i2 : t =
    let* ((l1, r1) as vi1) = i1 in
    let* ((l2, r2) as vi2) = i2 in
    if is_intersect vi1 vi2 then Some (max l1 l2, min r1 r2) else None

  let union i1 i2 =
    let* ((l1, r1) as vi1) = i1 in
    let* ((l2, r2) as vi2) = i2 in
    if is_intersect vi1 vi2 then Some (min l1 l2, max r1 r2) else None

  let add = liftA2 (fun (l1, r1) (l2, r2) -> (l1 + l2, r1 + r2))
  let minus = liftA2 (fun (l1, r1) (l2, r2) -> (l1 - r2, r2 - l1))

  let mul =
    liftA2 (fun (a, b) (c, d) -> minmax4 (a * c) (a * d) (b * c) (b * d))

  let div =
    liftA2 (fun (a, b) (c, d) -> minmax4 (a / c) (a / d) (b / c) (b / d))

  let to_string = function
    | None -> "(empty)"
    | Some (l, r) -> "[" ^ string_of_int l ^ " , " ^ string_of_int r ^ "]"

  let format fmt = function
    | None -> Format.pp_print_string fmt "(empty)"
    | Some (l, r) ->
        Format.(
          pp_print_char fmt '[';
          pp_print_int fmt l;
          pp_print_string fmt " , ";
          pp_print_int fmt r;
          pp_print_string fmt "]")
end
