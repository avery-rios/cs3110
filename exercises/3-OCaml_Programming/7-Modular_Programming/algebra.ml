module type Ring = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val of_int : int -> t
end

module type Field = sig
  include Ring
  val of_int : int -> t
end

module IntRingImpl = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = ( + )
  let ( ~- ) = ( ~- )
  let ( * ) = ( * )
  let to_string = string_of_int
  let of_int n = n
end

module IntRing : Ring = IntRingImpl

module IntFieldImpl = struct
  include IntRingImpl
  let ( / ) = ( / )
end

module IntField : Field = IntFieldImpl

module FloatRingImpl = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = ( +. )
  let ( ~- ) = ( ~-. )
  let ( * ) = ( *. )
  let to_string = string_of_float
  let of_int n = float_of_int n
end

module FloatRing : Ring = FloatRingImpl

module FloatFieldImpl = struct
  include FloatRingImpl
  let ( / ) = ( /. )
end

module FloatField : Field = FloatFieldImpl

module Rational (F : Ring) = struct
  type t = F.t * F.t
  let zero = (F.zero, F.one)
  let one = (F.one, F.one)
  let ( + ) (a, b) (c, d) = F.((a * d) + (c * b), b * d)
  let ( ~- ) (a, b) = (F.(-a), b)
  let ( * ) (a, b) (c, d) = F.(a * c, b * d)
  let ( / ) (a, b) (c, d) = F.(a * d, b * c)
  let to_string (a, b) = F.(to_string a ^ "/" ^ to_string b)
  let of_int n = F.(of_int n, one)
end

module IntRational : Field = Rational(IntRing)

module FloatRational : Field = Rational(FloatRing)

module RingOfInt (R : Ring) = struct
  let rec of_int_abs n =
    if n = 0 then R.zero
    else begin
      let r1 = of_int_abs (n lsr 1) in
      if n land 1 = 0 then R.( r1 + r1 )
      else R.( r1 + r1 + one ) 
    end
  let of_int n =
    let r = of_int_abs (abs n) in
    if n < 0 then R.(~- r) else r
end
