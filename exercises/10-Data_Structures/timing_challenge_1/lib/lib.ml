open Lwt.Infix

let delay_then_print () =
  Lwt_unix.sleep 3. >>= (fun () -> Lwt_io.print "done")