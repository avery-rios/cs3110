open OUnit2
open Sets.Set

let _ =
  run_test_tt_main
    ("glass box"
    >::: [
           ("mem empty" >:: fun _ -> assert_equal false ListSet.(empty |> mem 1));
           ( "mem add" >:: fun _ ->
             assert_equal true ListSet.(empty |> add 1 |> mem 1) );
           ( "elements empty" >:: fun _ ->
             assert_equal [] ListSet.(empty |> elements) );
           ( "emelents add" >:: fun _ ->
             assert_equal [ 1; 2 ]
               ListSet.(empty |> add 2 |> add 1 |> add 1 |> elements) );
         ])
