open Lwt.Infix
open Lwt_io
open Lwt_unix

(** [log ()] is a promise for an [input_channel] that reads from
the file named "log". *)
let log () : input_channel Lwt.t =
  openfile "log" [ O_RDONLY ] 0 >>= fun fd -> Lwt.return (of_fd ~mode:input fd)

(** [loop ic] reads one line from [ic], prints it to stdout,
then calls itself recursively. It is an infinite loop. *)
let rec loop (ic : input_channel) =
  read_line_opt ic >>= function
  | Some l -> Lwt_io.printl l >>= fun _ -> loop ic
  | None -> Lwt.return ()

(* hint: use [Lwt_io.read_line] and [Lwt_io.printlf] *)

(** [monitor ()] monitors the file named "log". *)
let monitor () : unit Lwt.t = log () >>= loop

let main = monitor
let _ = Lwt_main.run (main ())
