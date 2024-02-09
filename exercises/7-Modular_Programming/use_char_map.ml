module CharMap = Map.Make(Char)

let map =
  CharMap.(empty
  |> add 'A' "Alpha"
  |> add 'E' "Echo"
  |> add 'S' "Sierra"
  |> add 'D' "Victor")
let be = CharMap.find 'E' map
let withoutA = CharMap.remove 'A' map
let existsA = CharMap.mem 'A' withoutA
let assoc_of_map = CharMap.bindings map