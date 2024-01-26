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

val from_json : Yojson.Basic.t -> adventure
(** [from_json t] is the ocaml value of adventure description [t] *)
