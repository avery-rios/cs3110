let lazy_hello = lazy (print_string "Hello lazy world")
let ( &&& ) c1 c2 = if Lazy.force c1 then Lazy.force c2 else false
