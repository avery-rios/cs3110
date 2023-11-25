let rec is_desc last lst =
  match lst with
  | [] -> true
  | h :: t -> last >= h && is_desc h t

let rec is_insc last lst =
  match lst with
  | [] -> true
  | h :: t ->
      if last <= h then is_insc h t
      else is_desc h t

let is_unimodal lst =
  match lst with
  | [] -> true
  | h :: t -> is_insc h t