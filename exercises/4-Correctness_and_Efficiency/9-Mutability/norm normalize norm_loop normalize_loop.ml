(* AF: the float array [| x1; ...; xn |] represents the
   *
    vector (x1, ..., xn)
   * RI: the array is non-empty *)
type vector = float array

let norm v = Array.fold_left (fun acc i -> acc +. (i *. i)) 0. v |> sqrt

let normalize v =
  let n = norm v in
  Array.iteri (fun idx vi -> v.(idx) <- vi /. n) v

let norm_loop v =
  let acc = ref 0. in
  for i = 0 to Array.length v - 1 do
    acc := !acc +. v.(i)
  done;
  sqrt !acc

let normalize_loop v =
  let n = norm v in
  for i = 0 to Array.length v - 1 do
    v.(i) <- v.(i) /. n
  done
