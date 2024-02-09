let x = ref 0
let y = x
let z = ref 0

let _ =
  assert (x == y);
  assert (x != z);
  assert (x = y);
  assert (x = z);
  assert ((x := 1) = ());
  assert (x = y);
  assert (x <> z)
