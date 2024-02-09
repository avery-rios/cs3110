type 'a sequence = Cons of (unit -> 'a * 'a sequence)

let hd (Cons f) = fst (f ())
let tl (Cons f) = snd (f ())

let nats =
  let rec from i = Cons (fun () -> (i, from (i + 1))) in
  from 0

let rec map f (Cons s) =
  Cons
    (fun () ->
      let h, t = s () in
      (f h, map f t))
