type 'a t

val empty : 'a t

val insert : 'a -> 'a t -> 'a t
(** [insert a (insert b s) = insert b (insert a s)] *)

val is_empty : 'a t -> bool
(** - [is_empty empty = true]
    - [is_empty (insert e b) = false] *)

val mult : 'a -> 'a t -> int
(** - [mult a empty = 0]
    - [mult a (insert e s) = mult s + 1] if [a = e]
    - [mult a (insert e s) = mult s] if [a != e] *)

val remove : 'a -> 'a t -> 'a t
(** - [remove a empty = empty]
    - [remove a (insert e s) = s] if [a = e]
    - [remove a (insert e s) = insert e (remove a s)] if [a != e] *)
