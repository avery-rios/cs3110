let cube x = x *. x *. x

let sign x =
  if x > 0 then 1
  else if x == 0 then 0
  else -1

let _ = assert (sign 10 = 1)
let _ = assert (sign 0 = 0)
let _ = assert (sign (-5) = -1)

let aria r = 3.14 *. r *. r