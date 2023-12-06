module type ToString = sig
  type t
  val to_string : t -> string
end

module Print (M : ToString) = struct
  let print v = print_string (M.to_string v)    
end

module Int = struct
  type t = int
  let to_string = string_of_int
end

module PrintInt = Print(Int)

module MyString = struct
  type t = string
  let to_string v = v
end

module PrintString = Print(MyString)

module StringWithPrint = struct
  include MyString
  include PrintString 
end