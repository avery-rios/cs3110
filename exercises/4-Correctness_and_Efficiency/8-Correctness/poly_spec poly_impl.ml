(** [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
  type t
  (** [t] is the type of polynomials *)

  val const : int -> t
  (** [const c] is polynomial [c] *)

  val shift : int -> t -> t
  (** [shift c p] is polynomial $ c + x*p $*)

  val add : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t

  val coeff : int -> t -> int
  (** [coeff i p] is coefficient of [x^i] in p.
      Example:
        - if [p] represents $x + 1$, [coeff 2 p] is [0]
      Requires: i >= 0 *)

  val degree : t -> int
  val to_string : t -> string

  val eval : int -> t -> int
  (** [eval x p] is [p] evaluated at [x]. Example: if [p] represents
  $3x^3 + x^2 + x$, then [eval 10 p] is [3110]. *)
end

module PolyImpl : Poly = struct
  type t = int * int list
  (** [(c, [a_1; ...; a_n])] represents polynomial [c + a_1x + ... + a_n x^n].
      RI: [a_n != 0] *)

  let const c = (c, [])
  let shift c (c1, a) = (c, c1 :: a)

  let rec add_pol a b =
    match (a, b) with
    | [], b -> b
    | a, [] -> a
    | ah :: at, bh :: bt -> (ah + bh) :: add_pol at bt

  let coeff n p =
    if n = 0 then fst p
    else Option.value (List.nth_opt (snd p) (n - 1)) ~default:0

  let degree (_, p) = List.length p

  let to_string (c, a) =
    List.fold_left
      (fun (i, s) a ->
        (i + 1, string_of_int a ^ "x^" ^ string_of_int i ^ " + " ^ s))
      (1, string_of_int c)
      a
    |> snd

  let eval x (c, a) = List.fold_right (fun ax v -> ax + (x * v)) (c :: a) 0
  let add (c1, a1) (c2, a2) = (c1 + c2, add_pol a1 a2)
  let neg (c, a) = (-c, List.map ( ~- ) a)

  (** [mul_add v p1 p2] is [v*p1 + p2] *)
  let rec mul_add v p1 p2 =
    match (p1, p2) with
    | [], p2 -> p2
    | p1, [] -> List.map (( * ) v) p1
    | h1 :: t1, h2 :: t2 -> ((h1 * v) + h2) :: mul_add v t1 t2

  let rec drop_zero = function
    | [] -> []
    | 0 :: t -> ( match drop_zero t with [] -> [] | ts -> 0 :: ts)
    | h :: t -> h :: drop_zero t

  let mul (c1, a1) (c2, a2) =
    let c, a =
      List.fold_right
        (fun ax (c, a) -> (ax * c2, mul_add ax a2 (c :: a)))
        (c1 :: a1) (0, [])
    in
    (c, drop_zero a)
end
