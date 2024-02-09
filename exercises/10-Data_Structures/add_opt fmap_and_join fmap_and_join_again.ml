module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module MaybeImpl = struct
  type 'a t = 'a option

  let return x = Some x
  let ( >>= ) m f = match m with Some x -> f x | None -> None
end

module Maybe : Monad = MaybeImpl

let add ma mb =
  let open Maybe in
  ma >>= fun a ->
  mb >>= fun b -> return (a + b)

module type ExtMonad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module MkExtMonad (M : Monad) : ExtMonad with type 'a t = 'a M.t = struct
  include M

  let ( >>| ) a f = a >>= fun v -> return (f v)
  let join m = m >>= fun i -> i
end

module MaybeExt : ExtMonad = struct
  include MaybeImpl

  let ( >>| ) m f = match m with Some v -> Some (f v) | None -> None
  let join m = match m with Some v -> v | None -> None
end
