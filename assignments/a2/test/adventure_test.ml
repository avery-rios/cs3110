open OUnit2

let _ =
  run_test_tt_main
    ("Adventure test suite"
    >::: [ Command_test.test_suites; State_test.test_suites ])
