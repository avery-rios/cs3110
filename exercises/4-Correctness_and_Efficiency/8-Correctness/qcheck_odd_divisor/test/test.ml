open QCheck
open Odd_div

let _ =
  QCheck_runner.run_tests_main
    [
      Test.make ~count:1000 (QCheck.make ~print:Print.int Gen.nat) (fun i ->
          let d = odd_divisor i in
          d mod 2 = 1 && i mod d = 0);
    ]
