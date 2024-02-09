type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let e_terms x =
  let rec from i lst =
    let cur = lst *. x /. float_of_int i in
    Cons (cur, fun () -> from (i + 1) cur)
  in
  Cons (1.0, fun () -> from 1 1.0)

let total (Cons (h, t)) =
  let rec from lst (Cons (h, t)) =
    let cur = lst +. h in
    Cons (cur, fun () -> from cur (t ()))
  in
  from h (t ())

let within eps (Cons (h, t)) =
  let rec find lst (Cons (h, t)) =
    if abs_float (h -. lst) < eps then h else find h (t ())
  in
  find h (t ())

let e x eps = e_terms x |> total |> within eps

let within_rel eps (Cons (h, t)) =
  let rec find lst (Cons (h, t)) =
    if abs_float ((h -. lst) /. min h lst) < eps then h else find h (t ())
  in
  find h (t ())

let e' x eps = e_terms x |> total |> within eps
