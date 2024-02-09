```ocaml
let f x = snd ((fun x -> x, x) (fst x))
```

Type: `'a * 'b -> 'a`

Proposition: $A \land B \implies A$

Eval:
```
  (fun x -> snd ((fun x -> x, x) (fst x))) (1, 2)
--> (step application)
  (snd ((fun x -> x, x) (fst x))) {(1 , 2) / x}
= snd ((fun x -> x, x) (fst (1 , 2)))
--> (step fst)
  snd ((fun x -> x, x) 1)
--> (step application)
  snd ((x, x) {1 / x})
= snd (1, 1)
--> (step snd)
  1
```

Simplified:
```ocaml
let f x = fst x
```
