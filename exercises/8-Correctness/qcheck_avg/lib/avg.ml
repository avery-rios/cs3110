(** [avg [x1; ...; xn]] is [(x1 + ... + xn) / n].
Requires: the input list is not empty. *)
let avg lst =
  let rec loop (s, n) = function
    | [] -> (s, n)
    | [ h ] -> (s + h, n + 1)
    | h1 :: h2 :: t ->
        if h1 = h2 then loop (s + h1, n + 1) t else loop (s + h1 + h2, n + 2) t
  in
  let s, n = loop (0, 0) lst in
  float_of_int s /. float_of_int n
