let fifth lst = 
  if List.length lst < 5 then 0
  else List.nth lst 4

let desc_sorted lst = List.rev (List.sort Stdlib.compare lst)