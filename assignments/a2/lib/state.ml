module StrMap = Map.Make (String)
module StrSet = Set.Make (String)

type str_set = StrSet.t
type 'a str_map = 'a StrMap.t

module type IdT = sig
  type t = private string

  val of_string : string -> t
  val compare : t -> t -> int
end

module IdImpl () : IdT = struct
  type t = string

  let of_string x = x
  let compare = String.compare
end

exception InvalidDesc of string

module MakeId (I : IdT) = struct
  include I

  module Map = struct
    include Map.Make (I)

    let find_raise k t =
      match find_opt k t with
      | Some r -> r
      | None -> raise (InvalidDesc ((k :> string) ^ " is not defined"))
  end

  let map_of_list f l = List.to_seq l |> Seq.map f |> Map.of_seq

  module Set = Set.Make (I)

  let set_of_list l = List.map of_string l |> Set.of_list

  type name_map = t str_map
  (** map lowercase string to id  *)
end

module ItemIdM = IdImpl ()
module ItemId = MakeId (ItemIdM)
module RoomIdM = IdImpl ()
module RoomId = MakeId (RoomIdM)
module ExitIdM = IdImpl ()
module ExitId = MakeId (ExitIdM)

type item = { id : ItemId.t; (* description : string;  *) points : int }
type exit = { keys : ItemId.Set.t; room_id : RoomId.t }
type room_description = { required_items : ItemId.Set.t; text : string }

type room = {
  id : RoomId.t;
  points : int;
  descriptions : room_description list;
  exits : exit ExitId.Map.t;
  exit_name : ExitId.name_map;
  treasure_items : ItemId.Set.t;  (** items should be dropped in the room *)
}

type game_info = {
  items : item ItemId.Map.t;
  item_name : ItemId.name_map;
  win_score : int;
  win_message : string;
}

type room_state = { info : room; entered : bool; items : ItemId.Set.t }

type ext_state = { visited : str_set; locations : string str_map }
(** Only for display. map/set key is original string *)

(* [state] represents the state of an adventure. *)
(* You may define [state] to be whatever type you wish here. *)
type state = {
  info : game_info;
  ext : ext_state;
  other_rooms : room_state RoomId.Map.t;
  current_room : room_state;
  inventory : ItemId.Set.t;
  score : int;
  turns : int;
}

module Init = struct
  let item (i : File.item) : ItemId.t * item =
    let id = ItemId.of_string i.id in
    (id, { id; (* description = i.description; *) points = i.points })

  let game_info (a : File.adventure) : game_info =
    {
      items = ItemId.map_of_list item a.items;
      item_name =
        List.to_seq a.items
        |> Seq.map (fun (i : File.item) ->
               (String.lowercase_ascii i.id, ItemId.of_string i.id))
        |> StrMap.of_seq;
      win_score =
        List.(
          fold_left (fun a (i : File.item) -> a + i.points) 0 a.items
          + fold_left (fun a (r : File.room) -> a + r.points) 0 a.rooms);
      win_message = a.win_message;
    }

  let exit (e : File.exit) : ExitId.t * exit =
    let id = ExitId.of_string e.direction in
    ( id,
      {
        (* direction = e.direction; *)
        keys = ItemId.set_of_list e.keys;
        room_id = RoomId.of_string e.room_id;
      } )

  let room (r : File.room) : room =
    {
      id = RoomId.of_string r.id;
      points = r.points;
      descriptions =
        List.map
          (fun (d : File.description) ->
            {
              required_items = ItemId.set_of_list d.required_items;
              text = d.text;
            })
          r.descriptions;
      exits = ExitId.map_of_list exit r.exits;
      exit_name =
        List.to_seq r.exits
        |> Seq.map (fun (e : File.exit) ->
               (String.lowercase_ascii e.direction, ExitId.of_string e.direction))
        |> StrMap.of_seq;
      treasure_items = ItemId.set_of_list r.treasures;
    }

  let start_loc (loc : File.start_location list) =
    List.fold_left
      (fun map (l : File.start_location) ->
        let id = ItemId.of_string l.item in
        StrMap.update l.room
          (function
            | None -> Some (ItemId.Set.singleton id)
            | Some is -> Some (ItemId.Set.add id is))
          map)
      StrMap.empty loc

  let rooms (a : File.adventure) =
    let open RoomId in
    let start_loc = start_loc a.start_locations in
    let rooms =
      map_of_list
        (fun (r : File.room) ->
          let info = room r in
          let id = info.id in
          ( id,
            {
              info;
              entered = r.id = a.start_room;
              items =
                Option.value
                  (StrMap.find_opt r.id start_loc)
                  ~default:ItemId.Set.empty;
            } ))
        a.rooms
    in
    let start_room = RoomId.of_string a.start_room in
    (Map.find_raise start_room rooms, Map.remove start_room rooms)

  let start_score (items : item ItemId.Map.t) (current_room : room_state) =
    current_room.info.points
    + ItemId.Set.fold
        (fun iid acc ->
          if ItemId.Set.mem iid current_room.info.treasure_items then
            acc + (ItemId.Map.find_raise iid items).points
          else acc)
        current_room.items 0

  let init_state (a : File.adventure) : state =
    let info = game_info a in
    let current_room, other_rooms = rooms a in
    let score = start_score info.items current_room in
    {
      info;
      ext =
        {
          visited = StrSet.singleton a.start_room;
          locations =
            List.to_seq a.start_locations
            |> Seq.map File.(fun l -> (l.item, l.room))
            |> StrMap.of_seq;
        };
      other_rooms;
      current_room;
      inventory = ItemId.set_of_list a.start_inv;
      score;
      turns = 0;
    }
end

let init_state_val = Init.init_state
let init_state j = File.from_json j |> Init.init_state
let win_score s = s.info.win_score
let score s = s.score
let turns s = s.turns
let current_room_id s = (s.current_room.info.id : RoomId.t :> string)
let inv s = (ItemId.Set.elements s.inventory :> string list)
let visited s = StrSet.elements s.ext.visited
let locations s = StrMap.bindings s.ext.locations

type cmd_result =
  | CROk of state
  | CRItemNotFound of string
  | CRExitNotFound of string
  | CRRequireKeys of string list

let go_exit exit st =
  let open RoomId in
  let next_room = Map.find_raise exit.room_id st.other_rooms in
  {
    st with
    ext =
      {
        st.ext with
        visited = StrSet.add (next_room.info.id :> string) st.ext.visited;
      };
    other_rooms = Map.add st.current_room.info.id st.current_room st.other_rooms;
    current_room = { next_room with entered = true };
    turns = st.turns + 1;
    score =
      (if next_room.entered then st.score else st.score + next_room.info.points);
  }

let take_item iid (st : state) =
  let open ItemId in
  let current_room = st.current_room in
  let item = Map.find_raise iid st.info.items in
  {
    st with
    ext =
      {
        st.ext with
        locations =
          StrMap.add
            (item.id :> string)
            (current_room.info.id :> string)
            st.ext.locations;
      };
    current_room = { current_room with items = Set.add iid current_room.items };
    inventory = Set.remove iid st.inventory;
    turns = st.turns + 1;
    score =
      (if Set.mem iid current_room.info.treasure_items then
         st.score + item.points
       else st.score);
  }

let drop_item iid (st : state) =
  let open ItemId in
  let current_room = st.current_room in
  let item = Map.find_raise iid st.info.items in
  {
    st with
    ext =
      {
        st.ext with
        locations = StrMap.remove (item.id :> string) st.ext.locations;
      };
    current_room =
      { current_room with items = Set.remove iid current_room.items };
    inventory = Set.add iid st.inventory;
    turns = st.turns + 1;
    score =
      (if Set.mem iid current_room.info.treasure_items then
         st.score - item.points
       else st.score);
  }

let ( >>= ) = Option.bind

let do_opt c st =
  let open Command in
  match c with
  | CGo e -> (
      match
        StrMap.find_opt e st.current_room.info.exit_name >>= fun eid ->
        ExitId.Map.find_opt eid st.current_room.info.exits
      with
      | Some exit ->
          if ItemId.Set.subset exit.keys st.inventory then
            CROk (go_exit exit st)
          else
            CRRequireKeys
              (ItemId.Set.(diff exit.keys st.inventory |> elements)
                :> string list)
      | None -> CRExitNotFound e)
  | CTake i -> (
      match StrMap.find_opt i st.info.item_name with
      | Some iid when ItemId.Set.mem iid st.current_room.items ->
          CROk (drop_item iid st)
      | _ -> CRItemNotFound i)
  | CDrop i -> (
      match StrMap.find_opt i st.info.item_name with
      | Some iid when ItemId.Set.mem iid st.inventory -> CROk (take_item iid st)
      | _ -> CRItemNotFound i)
  | CQuit | CLook | CScore | CInventory | CTurns -> CROk st

let do' c st = match do_opt c st with CROk s -> s | _ -> st

let rec look_room_desc items ds =
  match ds with
  | [] -> None
  | h :: t ->
      if ItemId.Set.subset h.required_items items then Some h.text
      else look_room_desc items t

let description st =
  look_room_desc st.inventory st.current_room.info.descriptions

let win_message st = st.info.win_message
