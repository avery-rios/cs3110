open OUnit2
open Sets.Set

let uniq_list = List.sort_uniq Int.compare

let _ =
  run_test_tt_main
    ("black box"
    >::: [
           ( "elements empty" >:: fun _ ->
             assert_equal [] ListSet.(elements empty) );
           ("mem empty" >:: fun _ -> assert_equal false ListSet.(empty |> mem 1));
           ( "mem add" >:: fun _ ->
             assert_equal true ListSet.(empty |> add 1 |> mem 1) );
           ( "elements add" >:: fun _ ->
             assert_equal [ 1; 2 ]
               (ListSet.(empty |> add 1 |> add 1 |> add 2 |> elements)
               |> uniq_list) );
         ])
