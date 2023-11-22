(* Name:
 * NetID: 
 *)

let is_leap y = y mod 4 = 0 && (not (y mod 100 = 0) || y mod 400 = 0)

let valid_date y m d =
  y > 0 && 1 <= d && (
    if m = "Jan" || m = "Mar" || m = "May" || m = "Jul"
      || m == "Aug" || m == "Nov"
      then d <= 31
    else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Dec"
      then d <= 30
    else if m = "Feb"
      then d <= (if is_leap y then 29 else 28)
    else false)

(** Requires: [n] > 0 *)
let rec syr n =
  if n = 1 then 0
  else if n mod 2 = 0 then 1 + (syr (n / 2))
  else 1 + (syr (n * 3 + 1))

let rec sum n l acc =
  if n = 0 then acc
  else match l with
  | [] -> acc
  | h :: t -> sum (n - 1) t (acc + h)

let rec nacci_rec n k =
  if k < 1 then []
  else if k = 1 then [1]
  else let r = nacci_rec n (k - 1) in
  sum n r 0 :: r

(** Requires: [n] > 0 && [k] > 0 *)
let nacci n k = List.rev (nacci_rec n k)

(* The code [failwith "Unimplemented"] above is just
 * a placeholder that raises an exception.  You should
 * replace it with your own implementation.  We will 
 * cover exceptions a little bit later in the course. *)