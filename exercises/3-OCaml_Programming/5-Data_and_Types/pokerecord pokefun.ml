type poketype = Normal | Fire | Water

type pokeman = {name: string; hp : int; ptype: poketype}

let charizard = {
  name = "charizard";
  hp = 78;
  ptype = Fire
}

let squirtle = {
  name = "squirtle";
  hp = 44;
  ptype = Water
}

let rec max_hp_aux def lst =
  match lst with
  | [] -> def
  | h :: t ->
      if h.hp > def.hp then max_hp_aux h t
      else max_hp_aux def t

let max_hp = function
  | [] -> None
  | h :: t -> Some (max_hp_aux h t)