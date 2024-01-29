open OUnit2
open Search.Data

module type Tests = sig
  val tests : OUnit2.test list
end

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M : DictionaryMaker) = struct
  module KV = struct
    type t = int

    (** in reverse order *)
    let compare a b =
      let c = Int.compare a b in
      if c > 0 then `LT else if c < 0 then `GT else `EQ

    let format = Format.pp_print_int
  end

  open M (KV) (KV)

  let insert_c k v d = insert k v d |> rep_ok
  let remove_c k d = remove k d |> rep_ok
  let insert_list l v = List.fold_left (fun a (k, v) -> insert_c k v a) v l
  let unordered = [ (1, 0); (3, 0); (2, 0); (0, 0); (5, 0); (4, 0) ]

  let print_assoc_list is =
    "["
    ^ String.concat "; "
        (List.map
           (fun (k, v) -> "(" ^ string_of_int k ^ ", " ^ string_of_int v ^ ")")
           is)
    ^ "]"

  module Random = struct
    open QCheck2
    open QCheck2.Test

    let insert_rep_ok ls =
      ignore (List.fold_left (fun a i -> insert_c i 0 a) empty ls)

    type ins_del = IdInsert | IdRemove

    let ins_del_printer = function
      | IdInsert -> "IdInsert"
      | IdRemove -> "IdRemove"

    let ins_del_gen =
      Gen.(map (function true -> IdInsert | false -> IdRemove) bool)

    let insert_del_rep_ok ops =
      ignore
        (List.fold_left
           (fun a (op, v) ->
             match op with
             | IdInsert -> insert_c v 0 a
             | IdRemove -> remove_c v a)
           empty ops)

    let random_tests =
      [
        make ~name:"insert rep_ok"
          ~print:Print.(list int)
          Gen.(list nat)
          (fun ls ->
            insert_rep_ok ls;
            true);
        make ~name:"insert remove rep_ok"
          ~print:Print.(list (pair ins_del_printer int))
          Gen.(list (pair ins_del_gen nat))
          (fun ops ->
            insert_del_rep_ok ops;
            true);
      ]

    let random_failed =
      [
        "insert rep_ok"
        >::: [ ("f1" >:: fun _ -> insert_rep_ok [ 1; 0; -1; 0 ]) ];
        ("insert remove rep_ok"
        >:::
        let test name dat = name >:: fun _ -> insert_del_rep_ok dat in
        [
          test "f1"
            [ (IdInsert, 2); (IdInsert, 0); (IdInsert, 1); (IdRemove, 0) ];
          test "f2"
            [
              (IdInsert, 0);
              (IdInsert, 2);
              (IdRemove, 1);
              (IdInsert, 4);
              (IdInsert, 3);
              (IdRemove, 0);
            ];
          test "f3"
            [
              (IdInsert, 2);
              (IdInsert, 0);
              (IdInsert, 6);
              (IdInsert, 1);
              (IdInsert, 5);
              (IdInsert, 3);
              (IdInsert, 4);
              (IdRemove, 0);
            ];
        ]);
      ]
  end

  let tests =
    [
      ( "empty dict is empty" >:: fun _ ->
        assert_bool "empty dict shoule be empty" (is_empty empty) );
      ("empty dict has no bind" >:: fun _ -> assert_equal None (empty |> find 1));
      ("empty dict has zero size" >:: fun _ -> assert_equal 0 (empty |> size));
      ( "size of nonempty dict" >:: fun _ ->
        assert_equal 6 (empty |> insert_list unordered |> size) );
      ( "insert exist key update value" >:: fun _ ->
        assert_equal (Some 2) (empty |> insert_c 1 1 |> insert_c 1 2 |> find 1)
      );
      ( "removed element is not mem" >:: fun _ ->
        assert_equal false
          (empty |> insert_list [ (1, 1); (2, 1) ] |> remove_c 1 |> member 1) );
      ( "remove nonexist element is same dict" >:: fun _ ->
        let d = insert_list [ (1, 1); (2, 1); (3, 1); (4, 1) ] empty in
        assert_equal ~printer:print_assoc_list (to_list d)
          (remove_c 5 d |> to_list) );
      ( "fold process from least to greatest" >:: fun _ ->
        assert_equal ~printer:print_assoc_list
          [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0) ]
          (empty |> insert_list unordered |> fold (fun k v a -> (k, v) :: a) [])
      );
      ( "to_list is in order from least to greatest" >:: fun _ ->
        assert_equal ~printer:print_assoc_list
          [ (5, 0); (4, 0); (3, 0); (2, 0); (1, 0); (0, 0) ]
          (empty |> insert_list unordered |> to_list) );
      "failed quickcheck" >::: Random.random_failed;
      "quickcheck" >::: QCheck_runner.to_ounit2_test_list Random.random_tests;
    ]
end

module ListDictTest = DictTester (MakeListDictionary)
module TreeDictTest = DictTester (MakeTreeDictionary)

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)
let tests =
  [
    "list directory" >::: ListDictTest.tests;
    "tree directory" >::: TreeDictTest.tests;
  ]

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml. *)
