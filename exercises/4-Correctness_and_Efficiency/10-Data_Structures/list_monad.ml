module type ExtMonad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module ListMonad : ExtMonad = struct
  type 'a t = 'a list

  let return x = [ x ]
  let ( >>= ) l f = List.concat_map f l
  let ( >>| ) l f = List.map f l
  let join = List.concat
end
