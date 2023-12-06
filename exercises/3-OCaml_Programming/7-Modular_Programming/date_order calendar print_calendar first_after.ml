module Date = struct
  type t = {month : int; day : int}
  let compare d1 d2 =
    if d1.month <> d2.month then compare d1.month d2.month
    else compare d1.day d2.day
end

module DateMap = Map.Make (Date)

type calendar = string DateMap.t

let _ = 
  let open Date in
    DateMap.(empty
      |> add {month = 1; day = 30} "Birthday 1"
      |> add {month = 2; day = 28} "Borthday 2")

let print_calendar c =
  DateMap.iter (fun c d -> 
    print_int c.month;
    print_char '/';
    print_int c.day;
    print_string " ";
    print_string d) c

let first_after c d = snd (DateMap.find_first (fun c2 -> Date.compare c c2 < 0) d)