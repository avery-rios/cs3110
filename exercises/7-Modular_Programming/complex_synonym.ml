module type ComplexSig = sig
  type t = float * float

  val zero : t
  val add : t -> t -> t
end