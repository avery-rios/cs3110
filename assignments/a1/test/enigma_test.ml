open OUnit2
open Enigma

(*******************************************************************)
(* Helper values used throughout this test suite. *)
(*******************************************************************)
 
let rotor_id_wiring  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor_I_wiring   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_II_wiring  = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotor_III_wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let refl_B_wiring    = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

(*******************************************************************)
(* Tests for Part 1 *)
(*******************************************************************)

let index_tests = [
  "index A" >:: (fun _ -> assert_equal 0  (index 'A'));
  "index Z" >:: (fun _ -> assert_equal 25 (index 'Z'));
]

(* You do not need to construct a test sweep for [index].  
 * It's too simple of a function for that to be worthwhile. *)

(*******************************************************************)
(* Tests for Part 2 *)
(*******************************************************************)

(* README: In the test case list below, the sweep cases are the first five in
 * the list.  We ask you to follow that format as you complete the
 * rest of the test suite for all the other functions.  You are free
 * to move any provided test cases into the sweep, as long as you 
 * document why the test case is interesting. *)

let rotor_orient_wiring = "BACDEFGHIJKLMNOPQRSTUVWXYZ"

let test_rl name rotor top_letter input output =
  name >:: (fun _ -> assert_equal output (map_r_to_l rotor top_letter input))

let map_rl_tests = [
  (* Sweep case 1:  this is interesting because it tests a rotor whose wiring
   *   is as simple as possible:  every contact on the LHS connects directly
   *   to the corresponding contact on the RHS. *)
  "rl_id0" >:: (fun _ -> assert_equal 0 (map_r_to_l rotor_id_wiring 'A' 0));
  
  test_rl "rl_wrap_r" rotor_id_wiring 'B' 25 25;

  test_rl "rl_orientation1" rotor_orient_wiring 'A' 0 1;
  test_rl "rl_orientation2" rotor_orient_wiring 'B' 0 25;

  test_rl "rl_rotor3" rotor_III_wiring 'O' 14 17;

  (* Sweep case 2: TODO *)
   
  (* Sweep case 3: TODO *)
        
  (* Sweep case 4: TODO *)
  
  (* Sweep case 5: TODO *)
  
  (* Other test cases (not part of the sweep) *)      
  "rl_ex1" >:: (fun _ -> assert_equal 4 (map_r_to_l rotor_I_wiring  'A' 0));
  "rl_ex2" >:: (fun _ -> assert_equal 9 (map_r_to_l rotor_I_wiring  'B' 0));
]

let test_lr name rotor top_letter input output =
  name >:: (fun _ -> assert_equal output (map_l_to_r rotor top_letter input))

let map_lr_tests = [
  test_lr "id" rotor_id_wiring 'A' 0 0;

  test_lr "lr_ex4" rotor_I_wiring 'B' 0 21;

  test_lr "lr_orientation1" rotor_orient_wiring 'A' 0 1;
  test_lr "lr_orientation2" rotor_orient_wiring 'B' 0 25;

  test_lr "lr_rotor1" rotor_I_wiring 'F' 10 14;
  
  (* Other test cases (not part of the sweep) *) 
  "lr_ex3" >:: (fun _ -> assert_equal 20 (map_l_to_r rotor_I_wiring  'A' 0));
]


(*******************************************************************)
(* Tests for Part 3 *)
(*******************************************************************)

let test_refl name refl input output =
  name >:: (fun _ -> assert_equal output (map_refl refl input))

let map_refl_tests = [
  test_refl "refl_B1" refl_B_wiring 24 0;
  (* Other test cases (not part of the sweep) *) 
  "refl_B0" >:: (fun _ -> assert_equal 24 (map_refl refl_B_wiring 0));
]

(*******************************************************************)
(* Tests for Part 4 *)
(*******************************************************************)

let test_plug name plug input output =
  name >:: (fun _ -> assert_equal output (map_plug plug input))

let map_plug_tests = [
  test_plug "swap_xy" [('X', 'Y')] 'X' 'Y';
  test_plug "swap_xy" [('Y', 'X')] 'Y' 'X';

  test_plug "swap_snd" [('X', 'Y'); ('Z', 'A')] 'Z' 'A';
  
  (* Other test cases (not part of the sweep) *) 
  "plug_empty" >:: (fun _ -> assert_equal 'A' (map_plug [] 'A'));
]

(*******************************************************************)
(* Tests for Part 5 *)
(*******************************************************************)

let id_config = {
  refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  rotors = [];
  plugboard = [];
}

let rotor_I = {
  wiring = rotor_I_wiring;
  turnover = 'Q';
}

let rotor_II = {
  wiring = rotor_II_wiring;
  turnover = 'E';
}

let rotor_III = {
  wiring = rotor_III_wiring;
  turnover = 'V';
}

let cipher_char_ex_config = {
  refl = refl_B_wiring;
  rotors = [
    {rotor = rotor_I;   top_letter = 'A'};
    {rotor = rotor_II;  top_letter = 'A'};
    {rotor = rotor_III; top_letter = 'A'};
  ];
  plugboard = [];
}

let test_cipher_char name config input output =
  name >:: (fun _ -> assert_equal output (cipher_char config input))

let cipher_char_tests = [
  (* TODO: test sweep *) 
  
  (* Other test cases (not part of the sweep) *) 
  test_cipher_char "cipher_id" id_config 'A' 'A';
  "cipher_ex" >:: (fun _ -> assert_equal 'P' (cipher_char cipher_char_ex_config 'G'));
]

(*******************************************************************)
(* Tests for Part 6 *)
(*******************************************************************)

let step_ex_config = {
  refl = refl_B_wiring;
  rotors = [
    {rotor = rotor_III; top_letter = 'K'};
    {rotor = rotor_II;  top_letter = 'D'};
    {rotor = rotor_I;   top_letter = 'O'};
  ];
  plugboard = [];
}

let step_ex_config' = {
  refl = refl_B_wiring;
  rotors = [
    {rotor = rotor_III; top_letter = 'K'};
    {rotor = rotor_II;  top_letter = 'D'};
    {rotor = rotor_I;   top_letter = 'P'};
  ];
  plugboard = [];
}

let rec mk_rotors turns tops =
  match turns with
  | [] -> []
  | t :: rs ->
      { rotor = {wiring = rotor_id_wiring; turnover = t};
        top_letter = List.hd tops }
      :: mk_rotors rs (List.tl tops)

let mk_config turns tops = {
  refl = refl_B_wiring;
  rotors = mk_rotors turns tops;
  plugboard = [];
}

let rec step_seq n config =
  if n = 0 then []
  else let c = step config in
  c :: step_seq (n - 1) c 

let test_step name config input output =
  name >:: (fun _ -> assert_equal (mk_config config output) (step (mk_config config input)))

let string_of_top config =
  List.map (fun r -> r.top_letter) config.rotors
  |> List.to_seq
  |> String.of_seq

let string_of_top_seq configs =
  String.concat "" (
    "[" ::
    List.fold_right
      (fun c r ->  string_of_top c :: ";" :: r )
      configs
      ["]"]
  )

let test_steps name config input outputs =
  name >:: (fun _ ->
    assert_equal
      (List.map (mk_config config) outputs)
      (step_seq (List.length outputs) (mk_config config input))
      ~printer:string_of_top_seq
  )

let step_tests = [
  test_steps "step_ex1" ['V'; 'E'; 'Q'] ['K'; 'D'; 'O'] [
    ['K'; 'D'; 'P'];
    ['K'; 'D'; 'Q'];
    ['K'; 'E'; 'R'];
    ['L'; 'F'; 'S'];
    ['L'; 'F'; 'T'];
    ['L'; 'F'; 'U']
  ];

  test_steps "step_ex2" ['V'; 'E'; 'Q'] ['V'; 'D'; 'P'] [
    ['V'; 'D'; 'Q'];
    ['V'; 'E'; 'R'];
    ['W'; 'F'; 'S'];
    ['W'; 'F'; 'T']
  ];

  test_step "step_single" ['B'] ['C'] ['D'];
  test_step "step_single_back" ['B'] ['Z'] ['A'];

  test_step "step_none" [] [] [];
  (* TODO: test sweep *) 
  
  (* Other test cases (not part of the sweep) *) 
  "step_ex1a" >:: (fun _ -> assert_equal step_ex_config' (step step_ex_config));
]

(*******************************************************************)
(* Tests for Part 7 *)
(*******************************************************************)

let cipher_ex_config = {
  refl = refl_B_wiring;
  rotors = [
    {rotor = rotor_I;   top_letter = 'F'};
    {rotor = rotor_II;  top_letter = 'U'};
    {rotor = rotor_III; top_letter = 'N'};
  ];
  plugboard = ['A','Z'];
}

let test_cipher name config input output =
  name >:: (fun _ -> assert_equal output (cipher config input) ~printer:(fun i -> i))

let cipher_tests = [
  
  (* Other test cases (not part of the sweep) *) 
  test_cipher "ex" cipher_ex_config "YNGXQ" "OCAML";
]

let tests =
  "test suite for A1"  >::: [
    "index" >::: index_tests;
    "map_rl" >::: map_rl_tests;
    "map_lr" >::: map_lr_tests;
    "map_refl" >::: map_refl_tests;
    "map_plug" >::: map_plug_tests;
    "cipher_char" >::: cipher_char_tests;
    "step" >::: step_tests;
    "cipher" >::: cipher_tests;
  ]

let _ = run_test_tt_main tests
