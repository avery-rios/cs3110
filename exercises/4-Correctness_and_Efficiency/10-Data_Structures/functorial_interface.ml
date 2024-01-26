module CaseInsensitive = Hashtbl.Make (struct
  type t = string

  let hash s = String.hash (String.lowercase_ascii s)
  let equal s1 s2 = String.lowercase_ascii s1 = String.lowercase_ascii s2
end)
