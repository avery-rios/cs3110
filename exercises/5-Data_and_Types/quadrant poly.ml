let sign x : [> `Neg | `Pos | `Zero ] =
  if x > 0 then `Pos
  else if x = 0 then `Zero
  else `Neg

let quadrant ((x, y) : int * int) : [> `I | `II | `III | `IV ] option =
  match sign x, sign y with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None