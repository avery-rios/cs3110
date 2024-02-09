module BadTable = Hashtbl.Make (struct
  type t = int

  let hash _ = 0
  let equal i1 i2 = i1 = i2
end)

let _ =
  let tab = BadTable.create 4 in
  BadTable.add tab 0 0;
  BadTable.add tab 1 0;
  BadTable.add tab 2 0;
  BadTable.add tab 3 0;
  assert (
    let stat = BadTable.stats tab in
    stat.max_bucket_length = stat.num_bindings)
(* every binding is store in a single bucket *)
