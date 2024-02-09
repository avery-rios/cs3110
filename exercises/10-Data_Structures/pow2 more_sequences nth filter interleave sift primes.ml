type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec pow_b n = Cons (n, fun () -> pow_b (n * 2))
let pow2 = pow_b 1

let even =
  let rec even_from n = Cons (n, fun () -> even_from (n + 2)) in
  even_from 0

let lower_alpha =
  let rec from c =
    Cons
      ( Char.chr (c + int_of_char 'a'),
        fun () -> from (if c = 25 then 0 else c + 1) )
  in
  from 0

let coin =
  let rec from = Cons (Random.bool (), fun () -> from) in
  Random.self_init ();
  from

let rec nth (Cons (h, t)) n = if n = 0 then h else nth (t ()) (n - 1)

let rec filter f (Cons (h, t)) =
  if f h then Cons (h, fun () -> filter f (t ())) else filter f (t ())

let rec interleave (Cons (h1, t1)) s2 =
  Cons (h1, fun () -> interleave s2 (t1 ()))

let sift n = filter (fun i -> i mod n <> 0)

let prime =
  let rec nat_from n = Cons (n, fun () -> nat_from (n + 1)) in
  let rec from (Cons (h, t)) = Cons (h, fun () -> from (sift h (t ()))) in
  from (nat_from 2)
