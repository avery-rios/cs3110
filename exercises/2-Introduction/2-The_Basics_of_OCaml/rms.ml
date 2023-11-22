let rms x y = Float.sqrt ((x *. x +. y *. y ) /. 2.)

let _  = assert (Float.abs (rms 1. 1. -. 1.) < 1e-5)