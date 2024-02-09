parse `22`
```ocaml
Int 22
```


parse `1 + 2 + 3`
```ocaml
BinOp(Add, BinOp(Add, Int 1, Int 2), Int 3)
```

parse `let x = 2 in 20 + x`
```ocaml
Let ("x", Int 2, BinOp(Add, Int 2, Var "x"))
```

parse `3.14` during lexing, no lex rules about float point is defined

parse `3+` error during parsing, missing right operand
