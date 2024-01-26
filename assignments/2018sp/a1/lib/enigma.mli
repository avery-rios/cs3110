val index : char -> int

val map_r_to_l : string -> char -> int -> int
val map_l_to_r : string -> char -> int -> int

val map_refl : string -> int -> int

val map_plug : (char*char) list -> char -> char

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

val cipher_char : config -> char -> char

val step : config -> config

val cipher : config -> string -> string