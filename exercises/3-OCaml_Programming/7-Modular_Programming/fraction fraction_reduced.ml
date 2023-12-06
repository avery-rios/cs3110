module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t
  (** [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t
  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end

module FractionRecord = struct
  type t = {
    numerator: int;
    denominator: int;
  }

  let make n d = {
    numerator = n;
    denominator = d;
  }
  let numerator v = v.numerator
  let denominator v = v.denominator
  let to_string v = string_of_int v.numerator ^ "/" ^ string_of_int v.denominator
  let to_float v = float_of_int v.numerator /. float_of_int v.denominator
  let add a b = {
    numerator = a.numerator * b.denominator + b.numerator * a.denominator;
    denominator = a.denominator * b.denominator;
  }
  let mul a b = {
    numerator = a.numerator * b.numerator;
    denominator = a.denominator * b.denominator;
  }
end

module FractionMod : Fraction = FractionRecord

module FractionReducedImpl = struct
  type t = {
    numerator: int;
    denominator: int;
  }

  let rec gcd x y =
    if x = 0 then y
    else if (x < y) then gcd (y - x) x
    else gcd y (x - y)
  
  let make n d =
    assert (d <> 0);
    let (n1, pd) = if d < 0 then (- n, - d) else (n, d) in
    let g = gcd (abs n1) pd in
      { numerator = n1 / g; denominator = pd / g; }
  
  let numerator v = v.numerator
  let denominator v = v.denominator
  let to_string v = string_of_int v.numerator ^ "/" ^ string_of_int v.denominator
  let to_float v = float_of_int v.numerator /. float_of_int v.denominator
  let add a b =
    make
      (a.numerator * b.denominator + b.numerator * a.denominator)
      (a.denominator * b.denominator)
  let mul a b = make (a.numerator * b.numerator) (a.denominator * b.denominator)
end

module FractionRed : Fraction = FractionReducedImpl