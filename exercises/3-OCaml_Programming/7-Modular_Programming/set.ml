module StrSet = Set.Make(struct 
  type t = string
  let compare s1 s2 =
    String.(compare (lowercase_ascii s1) (lowercase_ascii s2))
end)