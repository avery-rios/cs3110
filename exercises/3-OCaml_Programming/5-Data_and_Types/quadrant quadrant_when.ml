type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign =
  if x > 0 then Pos
  else if x = 0 then Zero
  else Neg

let quadrant : int*int -> quad option = fun (x,y) ->
  match sign x, sign y with
  | (Pos, Pos) -> Some I
  | (Neg, Pos) -> Some II
  | (Neg, Neg) -> Some III
  | (Pos, Neg) -> Some IV
  | _ -> None

let quadrant_when : int*int -> quad option = function
  | (x, y) when x > 0 && y > 0 -> Some I
  | (x, y) when x < 0 && y > 0 -> Some II
  | (x, y) when x < 0 && y < 0 -> Some III
  | (x, y) when x > 0 && y < 0 -> Some IV
  | _ -> None