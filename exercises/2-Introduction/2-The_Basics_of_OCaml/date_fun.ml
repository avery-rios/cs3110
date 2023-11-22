let date_valid d m =
  1 <= d && (
    if m = "Feb" then d <= 28
    else if m = "Jan" || m = "Mar" || m = "May" || m = "Jul"
      || m == "Aug" || m == "Oct" || m == "Dec" then
        d <= 31
    else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov"
     then d <= 30
    else false
  )

let _ = assert (date_valid 2 "Jan")
let _ = assert (not (date_valid 31 "Feb"))
let _ = assert (date_valid 28 "Feb")
let _ = assert (not (date_valid 31 "Apr"))