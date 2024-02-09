type 'a t

val nil : 'a t
val cons : 'a -> 'a t -> 'a t

val append : 'a t -> 'a t -> 'a t
(** - [append nil l] = l]
    - [append (cons h t) l = cons h (append t l)] *)

val length : 'a t -> int
(** - [length nil = 0]
    - [length (cons a l) = 1 + length l] *)
