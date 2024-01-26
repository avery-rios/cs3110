type description = { required_items : string list; text : string }
type exit = { direction : string; room_id : string; keys : string list }

type room = {
  id : string;
  descriptions : description list;
  points : int;
  exits : exit list;
  treasures : string list;
}

type item = { id : string; description : string; points : int }
type start_location = { item : string; room : string }

type adventure = {
  rooms : room list;
  start_room : string;
  items : item list;
  start_locations : start_location list;
  start_inv : string list;
  win_message : string;
}

open Yojson.Basic.Util

let str_member n t = member n t |> to_string
let int_member n t = member n t |> to_int
let list_member n f t = member n t |> convert_each f

let to_description t : description =
  {
    required_items = list_member "requires" to_string t;
    text = str_member "text" t;
  }

let to_exit t : exit =
  {
    direction = str_member "direction" t;
    room_id = str_member "room_id" t;
    keys = list_member "keys" to_string t;
  }

let to_room t : room =
  {
    id = str_member "id" t;
    descriptions = list_member "descriptions" to_description t;
    points = int_member "points" t;
    exits = list_member "exits" to_exit t;
    treasures = list_member "treasure" to_string t;
  }

let to_item t : item =
  {
    id = str_member "id" t;
    description = str_member "description" t;
    points = int_member "points" t;
  }

let to_start_location t : start_location =
  { item = str_member "item" t; room = str_member "room" t }

let from_json t =
  {
    rooms = list_member "rooms" to_room t;
    start_room = str_member "start_room" t;
    items = list_member "items" to_item t;
    start_locations = list_member "start_locations" to_start_location t;
    start_inv = list_member "start_inv" to_string t;
    win_message = str_member "win_message" t;
  }
