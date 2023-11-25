open OUnit2
open Library

let make_fifth_test name exp input =
  name >:: fun _ -> assert_equal exp (fifth input) ~printer:string_of_int

let fifth_test = "fifth test" >::: [
  make_fifth_test "empty list"  0 [];
  make_fifth_test "less than five" 0 [1;2];
  make_fifth_test "exactly five element" 5 [1;2;3;4;5];
  make_fifth_test "more than five elements" 5 [1;2;3;4;5;6;7;8;9]
]

let make_desc_ord_test name exp input =
  name >:: fun _ -> assert_equal exp (desc_sorted input) 

let sort_test = "sort desc test" >::: [
  make_desc_ord_test "empty list" [] [];
  make_desc_ord_test "simple element" [1] [1];
  make_desc_ord_test "many element" [5;4;3;2;1] [1;3;4;2;5]
]

let _ = run_test_tt_main (test_list [fifth_test ; sort_test])