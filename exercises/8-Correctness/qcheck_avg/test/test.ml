let avg_check ls =
  float_of_int (List.fold_left ( + ) 0 ls) /. float_of_int (List.length ls)

let nonempty_list =
  QCheck.Gen.(map2 List.cons small_signed_int (small_list small_signed_int))

let _ =
  QCheck_runner.run_tests_main
    [
      QCheck.Test.make
        (QCheck.make ~print:QCheck.Print.(list int) nonempty_list)
        (fun l -> avg_check l = Avg.avg l);
    ]
