(* [command] represents a command input by a player. *)
(* You may redefine [command] to be synonymous with whatever
 * type you wish, but its name must not be changed.
 * It is okay to expose this type rather than make it abstract,
 * because it is not the representation type of a data structure. *)
type command =
  | CGo of string
  | CTake of string
  | CDrop of string
  | CQuit
  | CLook
  | CInventory
  | CScore
  | CTurns

(********************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will
 * use to test your submission.
 *)

val parse : string -> command
(** [parse str] is the command that represents player input [str]. 
  requires: [str] is one of the commands forms described in the 
    assignment writeup. *)

(* END DO NOT CHANGE
 ********************************************************)

(* You are free to add more code below *)
