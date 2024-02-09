```ocaml
let s x y z = (x z) (y z)
```

```
I |- fun x -> fun y -> fun z -> (x z) (y z) : t0 -> t1 -> t2 -> t5 -| {t3 = t4 -> t5, t0 = t2 -> t3, t1 = t2 -> t4}
  I, x: t0 |- fun y -> fun z -> (x z) (y z) : t1 -> t2 -> t5 -| {t3 = t4 -> t5, t0 = t2 -> t3, t1 = t2 -> t4}
    I, x: t0, y: t1 |- fun z -> (x z) (y z) : t2 -> t5 -| {t3 = t4 -> t5, t0 = t2 -> t3, t1 = t2 -> t4}
      I, x: t0, y: t1, z: t2 |- (x z) (y z) : t5 -| {t3 = t4 -> t5, t0 = t2 -> t3, t1 = t2 -> t4}
        I, x: t0, y: t1, z: t2 |- x z : t3 -| {t0 = t2 -> t3}
          I, x: t0, y: t1, z: t2 |- x : t0 -| {}
          I, x: t0, y: t1, z: t2 |- z : t2 -| {}
        I, x: t0, y: t1, z: t2 |- y z : t4 -| {t1 = t2 -> t4}
          I, x: t0, y: t1, z: t2 |- y : t1 -| {}
          I, x: t0, y: t1, z: t2 |- z : t2 -| {}
```

`('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c`
