open Lwt

let f n =
  let (p,r) = wait () in 
  let p' = p >>= (fun v -> Lwt_io.printf "%i" v) in
  wakeup r n; p'