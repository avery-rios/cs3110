(* A type exposed and defined in a .mli file must also be defined in
 * the corresponding .ml file.  So you must repeat the definition
 * of [command] here.  This helps OCaml achieve something called
 * "separate compilation", which you could google for.  Yes,
 * it's a little bit annoying, but there is a good reason for it. *)
type command =
  | CGo of string
  | CTake of string
  | CDrop of string
  | CQuit
  | CLook
  | CInventory
  | CScore
  | CTurns

let parse str =
  match String.trim str |> String.lowercase_ascii with
  | "quit" -> CQuit
  | "look" -> CLook
  | "inv" | "inventory" -> CInventory
  | "score" -> CScore
  | "turns" -> CTurns
  | s ->
      if String.starts_with ~prefix:"take " s then
        CTake (Str.string_after s 5 |> String.trim)
      else if String.starts_with ~prefix:"drop " s then
        CDrop (Str.string_after s 5 |> String.trim)
      else if String.starts_with ~prefix:"go " s then
        CGo (Str.string_after s 3 |> String.trim)
      else CGo s
