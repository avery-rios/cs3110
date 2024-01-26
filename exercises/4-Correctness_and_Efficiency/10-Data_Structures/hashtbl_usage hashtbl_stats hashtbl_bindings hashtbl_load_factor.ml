let tab = Hashtbl.create 16

let _ =
  List.iter
    (fun x -> Hashtbl.add tab x (string_of_int x))
    (List.init 31 (fun x -> x));
  assert (Hashtbl.find tab 10 = "10");
  assert (Hashtbl.find_opt tab 35 = None)

(* stats *)
let _ =
  let stat = Hashtbl.stats tab in
  assert (stat.num_buckets = 16);
  assert (stat.bucket_histogram.(1) = 3)

let bindings tab = Hashtbl.fold (fun k v l -> (k, v) :: l) tab []

let load_factor tab =
  let stat = Hashtbl.stats tab in
  float_of_int stat.num_bindings /. float_of_int stat.num_buckets

(* Add one more binding load factor increase to 2 *)
(* Add another binding, load factor decrease to about 1 *)
