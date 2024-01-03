open QCheck
open Lib

let list_5_10_gen = Gen.(list_size (int_range 5 10) (int_range 0 100))
let _ = Gen.generate1 list_5_10_gen
let _ = Gen.generate ~n:3 list_5_10_gen
let list_5_10_arb = QCheck.make ~print:Print.(list int) list_5_10_gen
let even_list = List.exists is_even

let _ =
  QCheck_runner.run_tests_main
    [ Test.make ~name:"even_list" ~count:1000 list_5_10_arb even_list ]
