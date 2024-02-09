type 'a tree
(** place holder*)

val num_vowels : string -> int
(** [num_vowels s] is the number of vowels in [s] *)

val is_sorted : 'a list -> bool
(** [is_sorted a] returns true if and only if list [a] is sorted, that is
  a_0 < a_1 < ... < a_n. And empty list is sorted.
  Example:
    - [is_sorted [] = true]
    - [is_sorted [1;2;3] = true]
    - [is_sorted [1;3;2] = false] *)

val sort : 'a list -> 'a list
(** [sort l] returns a list statisfy coditions
  - [length l = length (sort l)]
  - [is_sorted (sort l) = true]
  - forall [0 <= i < length l], [mem (nth i l) (sort l) = true] *)

val max : 'a list -> 'a
(** [max l] takes a nonempty list [l] and returns [a] statisfy
    - [mem a l = true]
    - forall [0 <= i < length l], [nth i l <= a]
  requires: [l] is not empty *)

val is_prime : int -> bool
(** [is_prime i = true] if and only if [i] is a prime number.  *)

val is_palindrome : string -> bool
(** [is_palindrome s = true] if and only if forall [0 <= i < length s]
    [get s i = get s (length - 1 - i)] *)

val second_largest : int list -> int
(** [second_largest l] returns [m] statisfy
      - [m < max l]
      - [mem m l = true]
      - forall [0 <= i < length l], [mem (nth i l) <= m || mem (nth i l) = max l]
    requires: [length l >= 2] *)

val depth : 'a tree -> int
(** [depth t] returns [d] statisfy
      - forall subtree [t1], [depth t1 < d]
      - for child [c], [d = depth c + 1] *)
