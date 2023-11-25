open OUnit2
open List_max_exn

let tests = "list max" >::: [
  "empty list" >:: (fun _ -> assert_raises (Failure "list_max") (fun _ -> list_max []));
  "singleton list" >:: (fun _ -> assert_equal 4 (list_max [4]));
  "many elem list" >:: (fun _ -> assert_equal 5 (list_max [1;2;3;4;5]))
]

let _ = run_test_tt_main tests