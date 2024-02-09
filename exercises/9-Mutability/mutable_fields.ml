type student = { name : string; mutable gpa : float }

let alice = { name = "Alice"; gpa = 3.7 }
let _ = alice.gpa <- 4.0
