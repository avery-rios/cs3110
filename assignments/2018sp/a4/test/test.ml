open OUnit2
open Jocalf.Ast
open Jocalf.Ast_factory
open Jocalf.Main
open Jocalf.Eval

let str_max_int = string_of_int max_int
let str_min_int = string_of_int min_int

(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)

(* all of these tests will currently fail, because you have not
   yet implemented interpretation of any of these syntactic forms *)
let tests =
  [
    ({|42|}, "42");
    ({|-1|}, "-1");
    (str_max_int, str_max_int);
    (str_min_int, str_min_int);
    ({|true|}, "true");
    ({|false|}, "false");
    ({|undefined|}, "undefined");
    ({|"xyzzy"|}, {|"xyzzy"|});
    ({|4/0|}, {|Exception: "Division by zero"|});
    ({|4 mod 0|}, {|Exception: "Division by zero"|});
    ({|let x = 0 in y|}, {|Exception: "Unbound variable"|});
    ({|throw 0|}, "Exception: 0");
    ({|fun (x) -> 0|}, "<closure>");
    ({|0 0|}, {|Exception: "Application: not a function"|});
    ( {|(fun (x) -> 0) 1 2|},
      {|Exception: "Application: wrong number of arguments"|} );
    ({|ref 0|}, "<location>");
    ({|1 := 0|}, {|Exception: "Assignment to non-location"|});
    ({|{"x":1}|}, "<object>");
  ]

let make_interp_expr_test name in_str out_str =
  name >:: fun _ ->
  assert_equal ~printer:(fun i -> i) out_str (interp_expr in_str)

let test_eval_expr name expr r =
  name >:: fun _ ->
  assert_equal ~printer:string_of_result r (eval_expr_init expr |> fst)

let ok_prim p = RValue (VPrim p)
let ok_bool b = ok_prim (PBool b)
let ok_int i = ok_prim (PInt i)
let ok_undef = ok_prim PUndef
let ok_str s = ok_prim (PString s)
let exn_prim p = RException (VPrim p)
let exn_str s = exn_prim (PString s)
let exn_int i = exn_prim (PInt i)
let plus_expr = make_binop BopPlus
let minus_expr = make_binop BopMinus
let assign_expr = make_binop BopAssign
let let_expr x v f = ELet (x, v, f (EVar x))
let try_expr b x h = ETry (b, x, h (EVar x))
let try_finally_expr b x h f = ETryFinally (b, x, h (EVar x), f)
let deref_expr = make_unop UopDeref

let fun_expr a b =
  make_fun (Array.to_list a) (b (Array.map (fun i -> EVar i) a))

let fix_expr f xs b =
  EFix (f, Array.to_list xs, b (EVar f) (Array.map (fun i -> EVar i) xs))

let simple_func = fun_expr [| "x" |] (fun a -> a.(0))

let simple_object =
  make_object [ ("i", EInt "1"); ("b", EBool true); ("s", EString "sss") ]

let seq_exprs l =
  List.fold_left (fun e i -> ESeq (e, i)) (List.hd l) (List.tl l)

let throw_int i = EThrow (EInt (string_of_int i))
let ext_is_int = EVar "is_int"

type equal = NotEq | WeakEq | StrictEq

let equal_tests =
  let test_e name f v =
    name
    >::: [
           test_eval_expr "equal" (f BopEq)
             (ok_bool
                (match v with
                | NotEq -> false
                | WeakEq -> true
                | StrictEq -> true));
           test_eval_expr "neq" (f BopNeq)
             (ok_bool
                (match v with
                | NotEq -> true
                | WeakEq -> false
                | StrictEq -> false));
           test_eval_expr "strict eq" (f BopEqStrict) (ok_bool (v = StrictEq));
           test_eval_expr "strict neq" (f BopNeqStrict)
             (ok_bool (v <> StrictEq));
         ]
  in
  let test name e1 e2 v = test_e name (fun op -> make_binop op e1 e2) v in
  [
    test "int = int" (EInt "1") (EInt "1") StrictEq;
    test "int <> int" (EInt "1") (EInt "2") NotEq;
    test "str = str" (EString "a") (EString "a") StrictEq;
    test "str <> str" (EString "a") (EString "b") NotEq;
    test "true = true" (EBool true) (EBool true) StrictEq;
    test "false <> true" (EBool false) (EBool true) NotEq;
    test "true = 1" (EBool true) (EInt "1") WeakEq;
    test "false = 0" (EBool false) (EInt "0") WeakEq;
    test "str 1 = 1" (EString "1") (EInt "1") WeakEq;
    test "str 2 <> 1" (EString "2") (EInt "1") NotEq;
    test "str 1 = true" (EString "1") (EBool true) WeakEq;
    test "undef = undef" EUndef EUndef StrictEq;
    test "str a = 1" (EString "a") (EInt "1") NotEq;
    test "str a = true" (EString "a") (EBool true) NotEq;
    test "ref 1 <> ref 2" (ERef (EInt "1")) (ERef (EInt "2")) NotEq;
    test "ref 1 = ref 1" (ERef (EInt "1")) (ERef (EInt "1")) WeakEq;
    test_e "ref 1 == ref 1"
      (fun op -> let_expr "x" (ERef (EInt "1")) (fun x -> make_binop op x x))
      StrictEq;
    test "fun <> fun" simple_func simple_func NotEq;
    test "simple obj = simple obj" simple_object simple_object StrictEq;
    test "obj ref = obj ref"
      (make_object [ ("r", ERef (EInt "1")) ])
      (make_object [ ("r", ERef (EInt "1")) ])
      WeakEq;
    test "object different fields name" simple_object (make_object []) NotEq;
    test "object different fields value"
      (make_object [ ("f", EInt "1") ])
      (make_object [ ("f", EInt "2") ])
      NotEq;
    test "object convert field value"
      (make_object [ ("f", EBool true) ])
      (make_object [ ("f", EInt "1") ])
      WeakEq;
    test "extern" ext_is_int ext_is_int NotEq;
  ]

type order = LT | EQ | GT

let compare_tests =
  let map_or f = function Some v -> ok_bool (f v) | None -> ok_bool false in
  let test name e1 e2 v =
    name
    >::: [
           test_eval_expr "lt" (make_binop BopLt e1 e2)
             (map_or (function LT -> true | EQ | GT -> false) v);
           test_eval_expr "leq" (make_binop BopLeq e1 e2)
             (map_or (function LT | EQ -> true | GT -> false) v);
           test_eval_expr "gt" (make_binop BopGt e1 e2)
             (map_or (function LT | EQ -> false | GT -> true) v);
           test_eval_expr "geq" (make_binop BopGeq e1 e2)
             (map_or (function LT -> false | EQ | GT -> true) v);
         ]
  in
  [
    test "a b" (EString "a") (EString "b") (Some LT);
    test "cc a" (EString "cc") (EString "a") (Some GT);
    test "1 1" (EInt "1") (EInt "1") (Some EQ);
    test "1 2" (EInt "1") (EInt "2") (Some LT);
    test "1 str 01" (EInt "1") (EString "01") (Some EQ);
    test "1 str 10" (EInt "1") (EString "10") (Some LT);
    test "str 10 1" (EString "10") (EInt "1") (Some GT);
    test "a 1" (EString "a") (EInt "1") None;
    test "undef undef" EUndef EUndef None;
    test "undef a" EUndef (EString "a") None;
    test "undef 1" EUndef (EInt "1") None;
    test "fun fun" simple_func simple_func None;
    test "fun 1" simple_func (EInt "1") None;
  ]

let bin_arith_tests =
  [
    ("plus"
    >:::
    let test name e1 e2 r = test_eval_expr name (plus_expr e1 e2) (ok_prim r) in
    [
      test "plus int string" (EInt "1") (EString "1") (PString "11");
      test "plus str str" (EString "2") (EString "11") (PString "211");
      test "plus int int" (EInt "1") (EInt "2") (PInt 3);
      test "plus int bool" (EInt "1") (EBool true) (PInt 2);
      test "plus undefined" EUndef (EInt "1") PUndef;
      test "plus int loc" (EInt "1") (ERef EUndef) PUndef;
    ]);
    "minus mul"
    >::: List.concat_map
           (fun (n, e1, e2, v_min, v_mul) ->
             [
               test_eval_expr ("minus " ^ n) (minus_expr e1 e2) (ok_prim v_min);
               test_eval_expr ("mul " ^ n)
                 (make_binop BopTimes e1 e2)
                 (ok_prim v_mul);
             ])
           [
             ("int int", EInt "2", EInt "10", PInt (-8), PInt 20);
             ("int string", EInt "2", EString "aa", PUndef, PUndef);
             ("int int_str", EInt "2", EString "1", PInt 1, PInt 2);
             ("int_str int", EString "2", EInt "1", PInt 1, PInt 2);
             ("string int", EString "aa", EInt "2", PUndef, PUndef);
             ("undef int", EUndef, EInt "10", PUndef, PUndef);
             ("undef undef", EUndef, EUndef, PUndef, PUndef);
           ];
    ("div mod"
    >::: List.concat_map
           (fun (n, e1, e2, v_d, v_m) ->
             [
               test_eval_expr ("div " ^ n) (make_binop BopDiv e1 e2)
                 (ok_prim v_d);
               test_eval_expr ("mod " ^ n) (make_binop BopMod e1 e2)
                 (ok_prim v_m);
             ])
           [
             ("int int", EInt "10", EInt "3", PInt 3, PInt 1);
             ("string int", EString "aa", EInt "3", PUndef, PUndef);
             ("int_str int", EString "10", EInt "3", PInt 3, PInt 1);
             ("undef undef", EUndef, EUndef, PUndef, PUndef);
           ]
         @
         let div_by_zero_exn = exn_str "Division by zero" in
         [
           test_eval_expr "div zero"
             (make_binop BopDiv (EInt "1") (EInt "0"))
             div_by_zero_exn;
           test_eval_expr "mod zero"
             (make_binop BopMod (EInt "2") (EInt "0"))
             div_by_zero_exn;
         ]);
  ]

let uop_tests =
  [
    ("neg"
    >:::
    let test name e v =
      test_eval_expr name (make_unop UopMinus e)
        (ok_prim (match v with Some i -> PInt i | None -> PUndef))
    in
    [
      test "int" (EInt "10") (Some (-10));
      test "true" (EBool true) (Some (-1));
      test "str 10" (EString "10") (Some (-10));
      test "str a" (EString "a") None;
      test "undef" EUndef None;
      test "loc" (ERef (EInt "10")) None;
      test "func" simple_func None;
      test "object" simple_object None;
      test "extern" ext_is_int None;
    ]);
    ("not"
    >:::
    let test name e v = test_eval_expr name (make_unop UopNot e) (ok_bool v) in
    [
      test "not true" (EBool true) false;
      test "not 1" (EInt "1") false;
      test "not 0" (EInt "0") true;
      test "empty str" (EString "") true;
      test "str a" (EString "a") false;
      test "undef" EUndef true;
      test "loc" (ERef (EInt "0")) false;
      test "fun" simple_func false;
      test "obj" simple_object false;
      test "extern" ext_is_int false;
    ]);
    ("typeof"
    >:::
    let test name e v =
      test_eval_expr name (make_unop UopTypeof e) (ok_prim (PString v))
    in
    [
      test "undefined" EUndef "undefined";
      test "bool" (EBool true) "bool";
      test "int 0" (EInt "0") "int";
      test "int 1" (EInt "1") "int";
      test "string" (EString "aa") "string";
      test "loc" (ERef (EInt "0")) "location";
      test "fun" simple_func "closure";
      test "object" simple_object "object";
      test "extern" ext_is_int "closure";
    ]);
    "deref"
    >::: [
           test_eval_expr "simple" (deref_expr (ERef (EInt "10"))) (ok_int 10);
           test_eval_expr "non location" (deref_expr (EInt "0")) ok_undef;
         ];
  ]

let short_circuit_tests =
  let test name e1 e2 r_and r_or =
    name
    >::: [
           test_eval_expr "and" (make_and e1 e2) r_and;
           test_eval_expr "or" (make_or e1 e2) r_or;
         ]
  in
  [
    test "true true" (EBool true) (EBool true) (ok_bool true) (ok_bool true);
    test "true false" (EBool true) (EBool false) (ok_bool false) (ok_bool true);
    test "false false" (EBool false) (EBool false) (ok_bool false)
      (ok_bool false);
    test "int bool convert" (EInt "1") (EInt "1") (ok_bool true) (ok_bool true);
    test "true throw" (EBool true) (EThrow (EInt "1")) (exn_int 1)
      (ok_bool true);
    test "false throw" (EBool false) (EThrow (EInt "2")) (ok_bool false)
      (exn_int 2);
  ]

let op_tests =
  [
    "binop arith" >::: bin_arith_tests;
    "compare" >::: compare_tests;
    "equal" >::: equal_tests;
    "unary op" >::: uop_tests;
    "short circuit" >::: short_circuit_tests;
    test_eval_expr "assign non location"
      (assign_expr (EInt "10") (EInt "0"))
      (exn_str "Assignment to non-location");
  ]

let let_expr_tests =
  let test name x e1 e2 v = test_eval_expr name (let_expr x e1 e2) v in
  [
    test "simple let" "x"
      (plus_expr (EInt "1") (EInt "2"))
      (fun x -> x)
      (ok_int 3);
    test "nested let" "x" (EInt "1")
      (fun x -> let_expr "y" (EInt "2") (fun y -> plus_expr x y))
      (ok_int 3);
    test "shadow let" "x" (EInt "1")
      (fun _ -> let_expr "x" (EInt "2") (fun x -> x))
      (ok_int 2);
  ]

let control_tests =
  [
    "if"
    >::: [
           test_eval_expr "if true"
             (make_if (EBool true) (EInt "1") (EInt "2"))
             (ok_int 1);
           test_eval_expr "if false"
             (make_if (EBool false) (EInt "1") (EInt "2"))
             (ok_int 2);
           test_eval_expr "if partial true"
             (make_if_partial (EBool true) (EInt "1"))
             (ok_int 1);
           test_eval_expr "if partial false"
             (make_if_partial (EBool false) (EInt "1"))
             ok_undef;
           test_eval_expr "if 0"
             (make_if (EInt "0") (EBool true) (EBool false))
             (ok_bool false);
           test_eval_expr "if 2"
             (make_if (EInt "2") (EInt "0") (EInt "1"))
             (ok_int 0);
         ];
    test_eval_expr "seq"
      (let_expr "x" (ERef (EInt "0")) (fun x ->
           ESeq (assign_expr x (EInt "1"), deref_expr x)))
      (ok_int 1);
    test_eval_expr "while"
      (let_expr "x" (ERef (EInt "0")) (fun x ->
           let_expr "i" (ERef (EInt "0")) (fun i ->
               ESeq
                 ( make_while
                     (make_binop BopLeq (deref_expr i) (EInt "100"))
                     (seq_exprs
                        [
                          assign_expr x
                            (plus_expr (deref_expr x) (deref_expr i));
                          assign_expr i (plus_expr (deref_expr i) (EInt "1"));
                        ]),
                   deref_expr x ))))
      (ok_int 5050);
  ]

let exception_tests =
  [
    test_eval_expr "throw" (make_throw (EInt "10")) (exn_int 10);
    test_eval_expr "try no exn"
      (make_try (EInt "10") "x" (EInt "1"))
      (ok_int 10);
    test_eval_expr "try exn"
      (try_expr (EThrow (EInt "10")) "x" (fun x -> plus_expr x (EInt "1")))
      (ok_int 11);
    test_eval_expr "nested throw"
      (try_expr (EThrow (EInt "10")) "x" (fun x ->
           EThrow (plus_expr x (EInt "1"))))
      (exn_int 11);
    test_eval_expr "try finally no exn"
      (make_try_finally (EInt "1") "x" EUndef (EInt "2"))
      (ok_int 1);
    test_eval_expr "try finally exn"
      (try_finally_expr (EThrow (EInt "1")) "x"
         (fun x -> plus_expr x (EInt "2"))
         EUndef)
      (ok_int 3);
    test_eval_expr "try finally throw"
      (make_try_finally (EInt "1") "x" EUndef (EThrow (EInt "2")))
      (exn_int 2);
    test_eval_expr "try catch throw finally"
      (make_try_finally (EThrow (EInt "1")) "x" (EThrow (EInt "2")) (EInt "3"))
      (exn_int 2);
    test_eval_expr "try catch final order"
      (let_expr "x" (ERef (EInt "0")) (fun x ->
           ESeq
             ( make_try_finally (EThrow (EInt "1")) "e"
                 (assign_expr x (EInt "2")) (assign_expr x (EInt "3")),
               deref_expr x )))
      (ok_int 3);
  ]

let fun_tests =
  let test_order name f a i = test_eval_expr name (EApp (f, a)) (exn_int i) in
  [
    test_eval_expr "app non function"
      (make_app (EInt "0") [ EInt "0" ])
      (exn_str "Application: not a function");
    test_eval_expr "wrong args number"
      (make_app (fun_expr [| "x"; "y" |] (fun _ -> EInt "1")) [ EInt "0" ])
      (exn_str "Application: wrong number of arguments");
    test_eval_expr "lex scope"
      (let_expr "x" (EInt "0") (fun x ->
           let_expr "f"
             (fun_expr [| "y" |] (fun a -> plus_expr x a.(0)))
             (fun f ->
               let_expr "x" (EInt "1") (fun _ -> make_app f [ EInt "1" ]))))
      (ok_int 1);
    test_order "fun first" (throw_int 0) [ throw_int 1; throw_int 2 ] 0;
    test_order "args left to right"
      (fun_expr [| "x"; "y" |] (fun a -> plus_expr a.(0) a.(1)))
      [ throw_int 0; throw_int 1 ]
      0;
    test_eval_expr "fix"
      (EApp
         ( fix_expr "f" [| "x" |] (fun f a ->
               let x = a.(0) in
               make_if
                 (make_binop BopGt x (EInt "0"))
                 (plus_expr x (EApp (f, [ minus_expr x (EInt "1") ])))
                 (EInt "0")),
           [ EInt "100" ] ))
      (ok_int 5050);
  ]

let object_tests =
  [
    test_eval_expr "object expr"
      (make_object
         [
           ("i", plus_expr (EInt "1") (EInt "2"));
           ("b", make_and (EBool true) (EBool false));
           ("s", plus_expr (EString "s") (EString "tr"));
         ])
      (RValue
         (VObject
            (object_of_fields
               [
                 ("i", VPrim (PInt 3));
                 ("b", VPrim (PBool false));
                 ("s", VPrim (PString "str"));
               ])));
    ("get field"
    >:::
    let test name obj f v = test_eval_expr name (make_get_field obj f) v in
    let test_s name obj f v = test name obj (EString f) v in
    [
      test_s "simple field" simple_object "i" (ok_int 1);
      test_s "non object" (EBool true) "f" ok_undef;
      test_s "nonexist field" simple_object "not_exist" ok_undef;
      test "computed field"
        (make_object [ ("str", EString "sss"); ("s", EString "s1") ])
        (plus_expr (EString "s") (EString "tr"))
        (ok_str "sss");
      test "field convert undefined"
        (make_object [ ("undefined", EString "s") ])
        (EFun ([], EInt "1"))
        (ok_str "s");
      test_eval_expr "object first"
        (make_get_field (throw_int 0) (throw_int 1))
        (exn_int 0);
    ]);
    ("update field"
    >:::
    let test name obj f e v =
      test_eval_expr name
        (make_update_field obj f e)
        (RValue (VObject (object_of_fields v)))
    in
    let test_s name obj f e v = test name obj (EString f) e v in
    let test_order name obj f e i =
      test_eval_expr name (make_update_field obj f e) (exn_int i)
    in
    [
      test_s "simple"
        (make_object [ ("old", EString "o") ])
        "old" (EString "n")
        [ ("old", VPrim (PString "n")) ];
      test_s "not exist"
        (make_object [ ("old", EString "o") ])
        "new" (EString "n")
        [ ("old", VPrim (PString "o")); ("new", VPrim (PString "n")) ];
      test_eval_expr "non object"
        (make_update_field EUndef (EString "f") (EInt "1"))
        (ok_int 1);
      test_order "object first" (throw_int 0) (throw_int 1) (throw_int 2) 0;
      test_order "field second" EUndef (throw_int 1) (throw_int 2) 1;
      test_order "value third" EUndef EUndef (throw_int 2) 2;
    ]);
    ("delete field"
    >:::
    let test name obj f v =
      test_eval_expr name (make_delete_field obj f)
        (RValue (VObject (object_of_fields v)))
    in
    let test_s name obj f v = test name (make_object obj) (EString f) v in
    let test_order name obj f i =
      test_eval_expr name (make_delete_field obj f) (exn_int i)
    in
    [
      test_s "simple"
        [ ("f0", EString "vf0"); ("f1", EString "vf1") ]
        "f1"
        [ ("f0", VPrim (PString "vf0")) ];
      test_s "not exist"
        [ ("f0", EString "vf0") ]
        "f1"
        [ ("f0", VPrim (PString "vf0")) ];
      test_eval_expr "not object"
        (make_delete_field (EInt "10") (EString "f0"))
        (ok_int 10);
      test_order "object first" (throw_int 0) (throw_int 1) 0;
      test_order "field second" EUndef (throw_int 1) 1;
    ]);
  ]

let _ =
  run_test_tt_main
    ("suite"
    >::: [
           "operators" >::: op_tests;
           "let expr" >::: let_expr_tests;
           "control flow" >::: control_tests;
           "exception" >::: exception_tests;
           "function" >::: fun_tests;
           "object" >::: object_tests;
           "interp expr"
           >::: List.mapi
                  (fun idx (i, o) ->
                    make_interp_expr_test ("test" ^ string_of_int idx) i o)
                  tests;
         ])
