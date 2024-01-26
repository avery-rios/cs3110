(*
 * CS 3110 Fall 2017 A2
 * Author:
 * NetID:
 *
 * Acknowledge here any contributions made to your solution that
 * did not originate from you or from the course staff:
 *
 *)

open Adventure

let check_win st =
  if State.score st >= State.win_score st then (
    ANSITerminal.(print_string [ green ] (State.win_message st));
    print_newline ();
    true)
  else false

let rec run won st =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | cmd -> (
      let open Command in
      match parse cmd with
      | (CTake _ as c) | (CDrop _ as c) | (CGo _ as c) -> next won st c
      | CLook ->
          ANSITerminal.(print_string [ blue ] "Room: ");
          print_endline (State.current_room_id st);
          State.description st |> Option.iter print_endline;
          run won st
      | CInventory ->
          ANSITerminal.(print_string [ cyan ] "Inventory: \n");
          State.inv st
          |> List.iter (fun i ->
                 print_string "- ";
                 print_endline i);
          run won st
      | CScore ->
          print_string "Score: ";
          print_int (State.score st);
          print_newline ();
          run won st
      | CTurns ->
          print_string "Turns: ";
          print_int (State.turns st);
          print_newline ();
          run won st
      | CQuit -> ())

and next won st c =
  let open State in
  match do_opt c st with
  | CROk st' -> run (won || check_win st') st'
  | CRItemNotFound i ->
      ANSITerminal.(print_string [ red ] ("Not found item " ^ i));
      print_newline ();
      run won st
  | CRExitNotFound e ->
      ANSITerminal.(print_string [ red ] ("Not found exit " ^ e));
      print_newline ();
      run won st
  | CRRequireKeys ks ->
      ANSITerminal.(
        print_string [ yellow ] "Go to this exit requires keys: ";
        List.iter
          (fun k ->
            print_string [ yellow ] k;
            print_string [ yellow ] ", ")
          ks);
      print_newline ();
      run won st

(* [play_game f] plays the game in adventure file [f]. *)
let play_game f =
  try
    let init = State.init_state (Yojson.Basic.from_file f) in
    run (check_win init) init
  with
  | State.InvalidDesc msg ->
      ANSITerminal.(
        print_string [ red ] ("Invalid adventure game description: " ^ msg))
  | Yojson.Json_error e ->
      ANSITerminal.(print_string [ red ] ("Error loading adventure file: " ^ e))

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(
    print_string [ red ] "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

let () = main ()
