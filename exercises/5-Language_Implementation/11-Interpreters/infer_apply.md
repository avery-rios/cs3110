```ocaml
let apply f x = f x
```

```
I |- fun f -> fun x -> f x : t0 -> t1 -> t2 -| {t0 = t1 -> t2}
  I, f: t0 |- fun x -> f x : t1 -> t2 -| {t0 = t1 -> t2}
    I, f: t0, x: t1 |- f x : t2 -| {t0 = t1 -> t2}
      I, f: t0, x: t1 |- f : t0 -| {}
      I, f: t0, x: t1 |- x : t1 -| {}
```

`('a -> 'b) -> 'a -> 'b`
