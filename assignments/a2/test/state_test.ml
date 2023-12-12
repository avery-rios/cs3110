open OUnit2
open Adventure
open State
open Command

(* let three_room = Yojson.Basic.from_file "../example/threerooms.json" *)

let one_room =
  Adventure.File.
    {
      rooms =
        [
          {
            id = "room1";
            descriptions = [ { required_items = []; text = "Room 1" } ];
            points = 10;
            exits = [];
            treasures = [ "item1" ];
          };
        ];
      start_room = "room1";
      items = [ { id = "item1"; description = "item1"; points = 100 } ];
      start_inv = [];
      start_locations = [ { room = "room1"; item = "item1" } ];
      win_message = "you beat one room";
    }

let simple_exit d =
  Adventure.File.{ keys = []; direction = "e_" ^ d; room_id = d }

let simple_room id points exits =
  Adventure.File.
    {
      id;
      descriptions = [];
      points;
      exits = List.map simple_exit exits;
      treasures = [];
    }

let simple_item id points : File.item = File.{ id; points; description = "" }

(** [only_rooms start rooms] is the adventure file with no items and [rooms] room.
    Start room is specified ny [start] *)
let only_rooms rooms start =
  File.
    {
      rooms;
      start_room = start;
      items = [];
      start_inv = [];
      start_locations = [];
      win_message = "";
    }

let two_rooms =
  only_rooms
    [
      simple_room "r1" 0 [ "r2" ];
      simple_room "r2" 10 [ "r1"; "r3" ];
      simple_room "r3" 100 [ "r2" ];
    ]
    "r1"

let cmd_case =
  let test_cmd name init c =
    name >:: fun _ ->
    match init_state_val init |> do_opt c with
    | CROk _ -> ()
    | _ -> failwith "command failed"
  in
  "case insensitive"
  >::: [
         test_cmd "go uppercase exit"
           (only_rooms
              [ simple_room "r1" 0 [ "R2" ]; simple_room "R2" 0 [] ]
              "r1")
           (CGo "e_r2");
         test_cmd "drop uppercase item"
           File.
             {
               rooms = [ simple_room "r1" 0 [] ];
               start_room = "r1";
               items = [ { id = "I1"; description = ""; points = 0 } ];
               start_inv = [ "I1" ];
               start_locations = [];
               win_message = "";
             }
           (CDrop "i1");
         test_cmd "take uppercase item"
           File.
             {
               rooms = [ simple_room "r1" 0 [] ];
               start_room = "r1";
               items = [ { id = "I1"; description = ""; points = 0 } ];
               start_inv = [];
               start_locations = [ { item = "I1"; room = "r1" } ];
               win_message = "";
             }
           (CTake "i1");
       ]

let cmd_invalid =
  let test_cmd name expect init c =
    name >:: fun _ -> assert_equal expect (init_state_val init |> do_opt c)
  in
  "robost"
  >::: [
         test_cmd "go noexist exit" (CRExitNotFound "e1") one_room (CGo "e1");
         test_cmd "go without key" (CRRequireKeys [ "i1" ])
           File.
             {
               rooms =
                 [
                   {
                     id = "r1";
                     descriptions = [];
                     points = 0;
                     exits =
                       [
                         { keys = [ "i1" ]; direction = "e_r2"; room_id = "r2" };
                       ];
                     treasures = [];
                   };
                   simple_room "r2" 0 [];
                 ];
               start_room = "r1";
               items = [ simple_item "i1" 0 ];
               start_inv = [];
               start_locations = [ { item = "i1"; room = "r1" } ];
               win_message = "";
             }
           (CGo "e_r2");
         test_cmd "take noexist item" (CRItemNotFound "i1") one_room
           (CTake "i1");
         test_cmd "drop noexist item" (CRItemNotFound "i1") one_room
           (CDrop "i1");
       ]

let invalid_desc =
  let test_desc name init cs =
    name >:: fun _ ->
    match
      List.fold_left
        (fun st c ->
          match do_opt c st with
          | CROk st' -> st'
          | _ -> failwith "command fail")
        (init_state_val init) cs
    with
    | _ -> ()
    | exception InvalidDesc _ -> ()
  in
  "invalid description"
  >::: [
         test_desc "go unknown exit"
           (only_rooms [ simple_room "r1" 0 [ "r2" ] ] "r1")
           [ CGo "e_r2" ];
         test_desc "take unknown item"
           File.
             {
               rooms =
                 [
                   {
                     id = "r1";
                     descriptions = [];
                     points = 0;
                     exits = [];
                     treasures = [ "i1" ];
                   };
                 ];
               start_room = "r1";
               items = [];
               start_inv = [];
               start_locations = [ { item = "i1"; room = "r1" } ];
               win_message = "";
             }
           [ CTake "i1" ];
       ]

let room_desc =
  let test_desc name expected init =
    name >:: fun _ -> assert_equal expected (init_state_val init |> description)
  in
  let adv_file desc ~inv ~room =
    File.
      {
        rooms =
          [
            {
              id = "r1";
              descriptions = desc;
              points = 0;
              exits = [];
              treasures = [];
            };
          ];
        start_room = "r1";
        items = List.map (fun i -> simple_item i 0) (inv @ room);
        start_inv = inv;
        start_locations = List.map (fun i -> { item = i; room = "r1" }) room;
        win_message = "";
      }
  in
  "room description"
  >::: [
         test_desc "no desc" None (only_rooms [ simple_room "r1" 0 [] ] "r1");
         test_desc "simple desc" (Some "d1")
           (adv_file
              [ File.{ required_items = []; text = "d1" } ]
              ~inv:[] ~room:[]);
         test_desc "match first desc" (Some "d1")
           (adv_file
              File.
                [
                  { required_items = []; text = "d1" };
                  { required_items = []; text = "d2" };
                  { required_items = []; text = "d3" };
                ]
              ~inv:[] ~room:[]);
         test_desc "one desc with key" None
           (adv_file
              File.[ { required_items = [ "i1" ]; text = "d1" } ]
              ~inv:[] ~room:[ "r1" ]);
         test_desc "desc with multiply key" (Some "d2")
           (adv_file
              File.
                [
                  { required_items = [ "i1"; "i3" ]; text = "d1" };
                  { required_items = [ "i1"; "i2" ]; text = "d2" };
                ]
              ~inv:[ "i2"; "i1" ] ~room:[ "i3" ]);
       ]

let test_score =
  let test_score name expected init f =
    name >:: fun _ -> assert_equal expected (f (init_state_val init) |> score)
  in
  let cmd_go d s = do' (CGo d) s in
  "score"
  >::: [
         test_score "init score" 110 one_room (fun s -> s);
         test_score "enter room" 10 two_rooms (cmd_go "e_r2");
         test_score "enter room and exit" 10 two_rooms (fun s ->
             cmd_go "e_r2" s |> cmd_go "e_r1");
         test_score "enter room twice" 110 two_rooms (fun s ->
             cmd_go "e_r2" s |> cmd_go "e_r3" |> cmd_go "e_r2");
       ]

let win_score =
  let test_win name expected init =
    name >:: fun _ -> assert_equal expected (init_state_val init |> win_score)
  in
  "win score"
  >::: [
         test_win "room score" 110 two_rooms;
         test_win "item score" 21
           File.
             {
               rooms = [ simple_room "r1" 1 [] ];
               start_room = "r1";
               items = [ simple_item "i1" 10; simple_item "i2" 10 ];
               start_inv = [];
               start_locations =
                 [ { item = "i1"; room = "r1" }; { item = "i2"; room = "r1" } ];
               win_message = "";
             };
       ]

type expected_room_stat = {
  score : int;
  current : string;
  visited : string list;
}

let rec run_cmd f stat = function
  | [] -> []
  | c :: cs ->
      let r = do_opt c stat in
      f r
      :: (match r with CROk st' -> run_cmd f st' cs | _ -> run_cmd f stat cs)

let cmd_test f name init cs =
  name >:: fun _ ->
  assert_equal (List.map snd cs)
    (run_cmd f (init_state_val init) (List.map fst cs))

let go_test =
  let test_go =
    cmd_test (function
      | CROk s ->
          { score = score s; current = current_room_id s; visited = visited s }
      | _ -> failwith "command failed")
  in
  "go"
  >::: [
         test_go "enter room" two_rooms
           [
             ( CGo "e_r2",
               { score = 10; current = "r2"; visited = [ "r1"; "r2" ] } );
           ];
         test_go "enter room and exit" two_rooms
           [
             ( CGo "e_r2",
               { score = 10; current = "r2"; visited = [ "r1"; "r2" ] } );
             ( CGo "e_r1",
               { score = 10; current = "r1"; visited = [ "r1"; "r2" ] } );
           ];
         test_go "enter room twice" two_rooms
           [
             ( CGo "e_r2",
               { score = 10; current = "r2"; visited = [ "r1"; "r2" ] } );
             ( CGo "e_r3",
               { score = 110; current = "r3"; visited = [ "r1"; "r2"; "r3" ] }
             );
             ( CGo "e_r2",
               { score = 110; current = "r2"; visited = [ "r1"; "r2"; "r3" ] }
             );
           ];
         test_go "enter exit with key"
           File.
             {
               rooms =
                 [
                   {
                     id = "r1";
                     points = 0;
                     descriptions = [];
                     exits =
                       [
                         {
                           direction = "e_r2";
                           keys = [ "i1"; "i2" ];
                           room_id = "r2";
                         };
                       ];
                     treasures = [];
                   };
                   simple_room "r2" 0 [];
                 ];
               start_room = "r1";
               items = [ simple_item "i1" 0; simple_item "i2" 0 ];
               start_inv = [ "i2"; "i1" ];
               start_locations = [];
               win_message = "";
             }
           [
             ( CGo "e_r2",
               { score = 0; current = "r2"; visited = [ "r1"; "r2" ] } );
           ];
       ]

type expected_item_stat = {
  score : int;
  inventory : string list;
  locations : (string * string) list;
}

let item_test =
  let test_items =
    cmd_test (function
      | CROk s ->
          { score = score s; inventory = inv s; locations = locations s }
      | _ -> failwith "command failed")
  in
  let adv_file treasures ~inv ~room =
    File.
      {
        rooms =
          [
            { id = "r1"; points = 0; descriptions = []; exits = []; treasures };
          ];
        start_room = "r1";
        items = List.map (fun (i, p) -> simple_item i p) (inv @ room);
        start_inv = List.map fst inv;
        start_locations =
          List.map (fun (i, _) -> { item = i; room = "r1" }) room;
        win_message = "";
      }
  in
  "take & drop"
  >::: [
         test_items "drop treasure"
           (adv_file [ "i1" ] ~inv:[ ("i1", 10) ] ~room:[])
           [
             ( CDrop "i1",
               { score = 10; inventory = []; locations = [ ("i1", "r1") ] } );
           ];
         test_items "take treasure"
           (adv_file [ "i1" ] ~inv:[] ~room:[ ("i1", 10) ])
           [ (CTake "i1", { score = 0; inventory = [ "i1" ]; locations = [] }) ];
         test_items "drop other"
           (adv_file [ "i2" ] ~inv:[ ("i1", 10) ] ~room:[ ("i2", 10) ])
           [
             ( CDrop "i1",
               {
                 score = 10;
                 inventory = [];
                 locations = [ ("i1", "r1"); ("i2", "r1") ];
               } );
           ];
         test_items "take other"
           (adv_file [ "i2" ] ~inv:[ ("i2", 10) ] ~room:[ ("i1", 10) ])
           [
             ( CTake "i1",
               { score = 0; inventory = [ "i1"; "i2" ]; locations = [] } );
           ];
       ]

let test_suites =
  "state"
  >::: [
         (* ( "max" >:: fun _ ->
            assert_equal 11111 (three_room |> init_state |> win_score) ); *)
         invalid_desc;
         "command" >::: [ cmd_case; cmd_invalid; go_test; item_test ];
         room_desc;
         win_score;
         test_score;
       ]
