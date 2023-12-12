(********************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will
 * use to test your submission.
 *)

type state
(** [state] is an abstract type representing the state of an adventure. *)

val init_state_val : File.adventure -> state
(** [init_state f] is the initial state of the game as determined by
    ocaml value [f] *)

val init_state : Yojson.Basic.t -> state
(** [init_state j] is the initial state of the game as
  determined by JSON object [j].
  requires: [j] represents an error-free adventure file. *)

val win_score : state -> int
(** [win_score s] is the winning score for the adventure whose current
  state is represented by [s]. *)

val score : state -> int
(** [score s] is the player's current score. *)

val turns : state -> int
(** [turns s] is the number of turns the player has taken so far. *)

val current_room_id : state -> string
(** [current_room_id s] is the id of the room in which the adventurer
  currently is. *)

val inv : state -> string list
(** [inv s] is the list of item id's in the adventurer's current inventory.
  No item may appear more than once in the list.  Order is irrelevant. *)

val visited : state -> string list
(** [visited s] is the list of id's of rooms the adventurer has visited.
  No room may appear more than once in the list.  Order is irrelevant. *)

val locations : state -> (string * string) list
(** [locations s] is an association list mapping item id's to the
  id of the room in which they are currently located.  Items
  in the adventurer's inventory are not located in any room.
  No item may appear more than once in the list.  The relative order
  of list elements is irrelevant, but the order of pair components
  is essential:  it must be [(item id, room id)]. *)

val do' : Command.command -> state -> state
(** [do' c st] is [st'] if doing command [c] in state [st] results 
  in a new state [st'], and return [st] if command is invalid
 *)

(* END DO NOT CHANGE
 ********************************************************)

(* You are free to add more code below *)

exception InvalidDesc of string
(** Invalid adventure description *)

type cmd_result =
  | CROk of state
  | CRItemNotFound of string
  | CRExitNotFound of string
  | CRRequireKeys of string list

val do_opt : Command.command -> state -> cmd_result
(** [do_opt c st] is [st'] if doing command [c] in state [st] results 
  in a new state [st'].  The function name [do_opt] is used because 
  [do] is a reserved keyword.  Define the "observable state" to
  be all the information that is observable about the state
  from the functions above that take a [state] as input.
    - The "go" (and its shortcuts), "take" and "drop" commands
      result in an appropriately updated [st'], as described in the
      assignment writeup, if their object is valid in 
      state [st].  If their object is invalid in state [st],
      returns error.
        + The object of "go" is valid if it is a direction by which
          the current room may be exited, and if the union of the items
          in the player's inventory and the current room contains
          all the keys required to move to the target room. 
        + The object of "take" is valid if it is an item in the
          current room.  
        + The object of "drop" is valid if it is an item in the
          current inventory.
    - The "quit", "look", "inventory", "inv", "score", and "turns"
      commands are always possible and leave the observable state unchanged.
    - The behavior of [do_opt] is unspecified if the command is
      not one of the commands given in the assignment writeup.
  effects: none.  [do_opt] is not permitted to do any printing as
    part of implementing the REPL.  [do_opt] is not permitted to cause
    the engine to terminate.  [do_opt] is not permitted to raise an exception
    unless the precondition is violated.
  requires: the input state was produced by [init_state] from an 
    error-free adventure file, or by repeated applications of [do']
    to such a state.
 *)

val description : state -> string option
val win_message : state -> string
