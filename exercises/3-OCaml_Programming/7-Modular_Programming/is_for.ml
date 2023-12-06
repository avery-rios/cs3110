module CharMap = Map.Make(Char)

let is_for = CharMap.mapi (fun c d -> String.make 1 c ^ " is for " ^ d)