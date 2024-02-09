```ocaml
let x = 5 in
let f y = x + y in
let g x = f x in
let x = 4 in
g 3
```

dynamic: 7

lexical: 8

```ocaml
let f y = x + y in
let x = 3 in
let y = 4 in
f 2
```

dynamic: 5

lexical: not valid
