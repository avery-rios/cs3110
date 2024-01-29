open OUnit2
open Search.Engine

let print_str_list ss =
  "[" ^ String.concat "; " (List.map (fun s -> "\"" ^ s ^ "\"") ss) ^ "]"

let test_word name exp w =
  name >:: fun _ ->
  assert_equal ~printer:print_str_list exp (Word.split_words w)

let word_test =
  [
    test_word "empty string" [] "";
    test_word "single char" [ "w" ] "w";
    test_word "single char preword" [] ".";
    test_word "only whitespace" [] "\n\t";
    test_word "preword with no boundary" [] "-----";
    test_word "single word" [ "word" ] "word";
    test_word "lower case" [ "abc" ] "ABC";
    test_word "no duplicate" [ "a" ] "a a a";
    test_word "sorted" [ "a"; "b"; "c" ] "a c b";
    test_word "split preword"
      [ "123"; "123an"; "a2341www"; "aa123an"; "aaa"; "w1aaa" ]
      "w1aaa\n123+   a2341www-\taaa\nAA123an\n123An";
    test_word "split boundary"
      [ "10"; "123"; "w1"; "w2" ]
      "--W1\n123\n+-w2.+   +10-\n";
    test_word "cornell sample"
      [
        "1807";
        "an";
        "any";
        "b";
        "can";
        "cornell";
        "e";
        "find";
        "found";
        "i";
        "in";
        "institution";
        "instruction";
        "person";
        "study";
        "where";
        "would";
      ]
      {|  "I would found an institution where
      any person can find instruction in
      any study." ---E. Cornell (b. 1807)|};
  ]

module TestEngine (E : Engine) = struct
  open E

  let case_insensitive =
    let test name f exp ws =
      name >:: fun _ ->
      assert_equal ~printer:print_str_list exp (f (index_of_words ws))
    in
    let f0_cornell = [ ("f0.txt", [ "cornell" ]) ] in
    [
      test "and"
        (fun idx -> and_not idx [ "Cornell" ] [])
        [ "f0.txt" ] f0_cornell;
      test "or" (fun idx -> or_not idx [ "Cornell" ] []) [ "f0.txt" ] f0_cornell;
      test "and not"
        (fun idx -> and_not idx [ "Cornell" ] [ "Cornell" ])
        [] f0_cornell;
      test "or not"
        (fun idx -> or_not idx [ "Cornell" ] [ "Cornell" ])
        [] f0_cornell;
    ]

  (** files with words ["a"],["b"],["c"]  *)
  let fs_abc =
    [
      ("none", []);
      ("a", [ "a" ]);
      ("b", [ "b" ]);
      ("c", [ "c" ]);
      ("ab", [ "a"; "b" ]);
      ("ac", [ "a"; "c" ]);
      ("bc", [ "b"; "c" ]);
      ("abc", [ "a"; "b"; "c" ]);
    ]

  let sort_str_list = List.sort String.compare

  let or_test =
    let test name exp (os, ns) ws =
      name >:: fun _ ->
      assert_equal ~printer:print_str_list (sort_str_list exp)
        (sort_str_list (or_not (index_of_words ws) os ns))
    in
    [
      test "single element" [ "a"; "ab"; "abc"; "ac" ] ([ "a" ], []) fs_abc;
      test "not exist " [] ([ "0" ], []) fs_abc;
      test "negate not exist" [ "a"; "ab"; "ac"; "abc" ] ([ "a" ], [ "0" ])
        fs_abc;
      test "or not exist" [ "a"; "ab"; "ac"; "abc" ] ([ "a"; "0" ], []) fs_abc;
      test "multi or"
        [ "a"; "ab"; "abc"; "ac"; "b"; "bc"; "c" ]
        ([ "a"; "b"; "c" ], [])
        fs_abc;
      test "not" [ "a"; "ab"; "b" ] ([ "a"; "b" ], [ "c" ]) fs_abc;
      test "not with char in ors" [ "b"; "bc" ] ([ "a"; "b" ], [ "a" ]) fs_abc;
    ]

  let and_test =
    let test name exp (ands, nots) ws =
      name >:: fun _ ->
      assert_equal ~printer:print_str_list (sort_str_list exp)
        (sort_str_list (and_not (index_of_words ws) ands nots))
    in
    [
      test "single element" [ "a"; "ab"; "ac"; "abc" ] ([ "a" ], []) fs_abc;
      test "not exist" [] ([ "0" ], []) fs_abc;
      test "and not exist" [] ([ "a"; "0" ], []) fs_abc;
      test "negate not exist" [ "a"; "ab"; "ac"; "abc" ] ([ "a" ], [ "0" ])
        fs_abc;
      test "and elem" [ "ab"; "abc" ] ([ "a"; "b" ], []) fs_abc;
      test "not" [ "ab" ] ([ "a"; "b" ], [ "c" ]) fs_abc;
      test "not with chars in ands" [] ([ "a"; "b" ], [ "a" ]) fs_abc;
    ]

  let tests =
    [
      "case insensitive" >::: case_insensitive;
      "or" >::: or_test;
      "and" >::: and_test;
    ]
end

module LETests = TestEngine (ListEngine)
module TETests = TestEngine (TreeEngine)

let tests =
  [
    "split word" >::: word_test;
    "list engine" >::: LETests.tests;
    "tree engine" >::: TETests.tests;
  ]

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml. *)
