(** [odd_divisor x] is an odd divisor of [x].
Requires: [x >= 0]. *)
let odd_divisor x =
  if x < 3 then 1
  else
    let rec search y =
      if y >= x then y (* exceeded upper bound *)
      else if x mod y = 0 then y (* found a divisor! *)
      else search (y + 2)
      (* skip evens *)
    in
    search 3
