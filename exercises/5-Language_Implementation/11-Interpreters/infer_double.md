```ocaml
let double f x = f (f x)
```

```
I |- fun f -> fun x -> f (f x) : t0 -> t1 -> t3 -| {t0 = t2 -> t3, t0 = t1 -> t2}
  I, f: t0 |- fun x -> f (f x) : t1 -> t3 -| {t0 = t2 -> t3, t0 = t1 -> t2}
    I, f: t0, x: t1 |- f (f x) : t3 -| {t0 = t2 -> t3, t0 = t1 -> t2}
      I, f: t0, x: t1 |- f : t0 -| {}
      I, f: t0, x: t1 |- f x : t2 -| {t0 = t1 -> t2}
        I, f: t0, x: t1 |- f : t0 -| {}
        I, f: t0, x: t1 |- x : t1 -| {}
```

`('a -> 'a) -> 'a -> 'a`
