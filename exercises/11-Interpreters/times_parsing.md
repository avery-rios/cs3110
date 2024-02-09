Change times to right assoc change `BinOp(Mul, BinOp(Mul, Int 1, Int 2), Int 3)`,
to `BinOp(Mul, Int 1, BinOp(Mul, Int 2, Int 3))`, cause `2*3` to be evaluated first.

Swap decl of `TIMES` with `PLUS` change `BinOp(Add, Int 1, BinOp(Mul, Int 2, Int 3))`
to `BinOp(Mul, BinOp(Add, Int 1, Int 2), Int 3)`, cause `1+2` to be evaluated first