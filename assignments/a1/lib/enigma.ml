(*
 * CS 3110 Fall 2017 A1
 * Author:
 * NetID:
 *
 * Acknowledge here any contributions made to your solution that
 * did not originate from you or from the course staff:
 *
 *)

(*********************************************************)
(* PART 1 *)
(*********************************************************)

(** [index c] is the 0-based index of [c] in the alphabet.
  requires: c is an uppercase letter in A..Z *)
let index c = Char.code c - Char.code 'A'

(** [of_char i] is the char of 0-based index [i]
		requires: i is 0..25 *)
let of_char i = Char.chr (i + Char.code 'A')
(* NOTE:  [failwith "Unimplemented"] raises an exception to indicate
   that the function has not been finished.  You should delete that
   line of code and replace it with your own code. *)

(*********************************************************)
(* PART 2 *)
(*********************************************************)

(** [add_offset_in offset x] is the position at which current would
		appear when current enters at input position [x] to a rotor whose 
		offset is [offset]*)
let add_offset_in offset x =
	let r = x + offset in
	if r > 25 then r - 26
	else r

(** [add_offset_out offset x] is the position at which current would
		appear when current exits at position [x] from a rotor whose offset
		is [offset]*)
let add_offset_out offset x =
	let r = x - offset in
	if r < 0 then r + 26
	else r

(** [map_r_to_l wiring top_letter input_pos] is the left-hand output position
 	 at which current would appear when current enters at right-hand input
 	 position [input_pos] to a rotor whose wiring specification is given by
 	 [wiring].  The orientation of the rotor is given by [top_letter], 
 	 which is the top letter appearing to the operator in the rotor's 
 	 present orientation.
 	 requires: 
 	  - [wiring] is a valid wiring specification.
 	  - [top_letter] is in 'A'..'Z'
 	  - [input_pos] is in 0..25
 *)
let map_r_to_l wiring top_letter input_pos =
	let offset = index top_letter in
	add_offset_in offset input_pos
	|> String.get wiring
	|> index
	|> add_offset_out offset

(** [reverse_wiring w] is the reverse mapping of [w].
		requires: [w] is a valid wiring specification *)
let reverse_wiring w = 
	List.init 26 (fun i -> String.index w (of_char i))

(** [map_l_to_r] computes the same function as [map_r_to_l], except
  for current flowing left to right. *)
let map_l_to_r wiring =
	let rev_wiring = reverse_wiring wiring in
	fun top_letter input_pos ->
		let offset = index top_letter in
		add_offset_in offset input_pos
		|> List.nth rev_wiring
		|> add_offset_out offset

(*********************************************************)
(* PART 3 *)
(*********************************************************)

(** [map_refl wiring input_pos] is the output position at which current would 
 	appear when current enters at input position [input_pos] to a reflector 
 	whose wiring specification is given by [wiring].
 	requires: 
 	 - [wiring] is a valid reflector specification.
 	 - [input_pos] is in 0..25
 *)
let map_refl wiring input_pos = index (String.get wiring input_pos)

(*********************************************************)
(* PART 4 *)
(*********************************************************)

(** [map_plug plugs c] is the letter to which [c] is transformed
 	by the plugboard [plugs].
 	requires:
 	 - [plugs] is a valid plugboard
 	 - [c] is in 'A'..'Z' 
 *)
let rec map_plug plugs c =
	match plugs with
	| [] -> c
	| (c1, c2) :: ps ->
			if c1 = c then c2
			else if c2 = c then c1
			else map_plug ps c

(*********************************************************)
(* PART 5 *)
(*********************************************************)

type rotor = {
	wiring : string;
	turnover : char;
}

type oriented_rotor = {
	rotor : rotor;
	top_letter : char;
}

type config = {
	refl : string;
	rotors : oriented_rotor list;
	plugboard : (char*char) list;
}

(** [cipher_rotor_rl rs c] is the ciphered [c] using rotors [rs] from
		right to left *)
let cipher_rotor_rl rs c =
	List.fold_right
		(fun r -> map_r_to_l r.rotor.wiring r.top_letter)
		rs
		c

(** [cipher_rotor_lr rs c] is the ciphered [c] using rotors [rs] from
		left to right *)
let cipher_rotor_lr rs c =
	List.fold_left
		(fun c r -> map_l_to_r r.rotor.wiring r.top_letter c)
		c
		rs

(** [cipher_char config c] is the letter to which the Enigma machine 
	ciphers input [c] when it is in configuration [config].
	requires:
	 - [config] is a valid configuration
	 - [c] is in 'A'..'Z'
 *)
let cipher_char config c =
	map_plug config.plugboard c
	|> index
	|> cipher_rotor_rl config.rotors
	|> map_refl config.refl
	|> cipher_rotor_lr config.rotors
	|> of_char
	|> map_plug config.plugboard 

(*********************************************************)
(* PART 6 *)
(*********************************************************)

(** [step_rotor r] is the stepped rotor of [r] *)
let step_rotor r =
	let top_index = index r.top_letter in
	{ r with
			top_letter =
				if top_index = 25 then 'A'
				else of_char (top_index + 1)
	}

(** [is_turnover r] tests if [r]'s top letter is its turnover *)
let is_turnover r = r.top_letter = r.rotor.turnover

(** [step_rotors rs] is the stepped lists of rotors from
		left to right that is not leftmost*)
let rec step_rotors rs =
	match rs with
	| [] -> []
	| [r] -> [step_rotor r]
	| r1 :: (r2 :: _ as rs) -> 
			(if is_turnover r2 || is_turnover r1
				then step_rotor r1
				else r1) :: step_rotors rs

(** [step config] is the new configuration to which the Enigma machine 
  transitions when it steps beginning in configuration [config].
  requires: [config] is a valid configuration
 *)
let step config = { config with
	rotors =
		match config.rotors with
		| [] -> []
		| [r] -> [step_rotor r]
		| r1 :: (r2 :: _ as rs) ->
				(if is_turnover r2
					then step_rotor r1
					else r1) :: step_rotors rs
}

(*********************************************************)
(* PART 7 *)
(*********************************************************)


(** [cipher config s] is the string to which [s] enciphers
  when the Enigma machine begins in configuration [config].
  requires: 
    - [config] is a valid configuration 
    - [s] contains only uppercase letters
 *)
let cipher config s =
	String.fold_left 
		(fun (cfg, s) c ->
			let cfg' = step cfg in
			(cfg', cipher_char cfg' c :: s))
		(config, [])
		s
	|> snd
	|> List.rev
	|> List.to_seq
	|> String.of_seq

