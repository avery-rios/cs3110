type prop =
  | Atom of string
  | Neg of prop
  | Conjunct of prop * prop
  | Disjunct of prop * prop
  | Implication of prop * prop

(* Induction principle
    forall properties P,
      if forall s, P (Atom s),
      and forall p, P(p) implies P(Neg p),
      and forall p1 p2, (P(p1) and P(p2)) implies P(Conjunct(p1, p2)),
      and forall p1 p2, (P(p1) and P(p2)) implies P(Disjunct(p1, p2)),
      and forall p1 p2, (P(p1) and P(p2)) implies P(Implication(p1, p2)),
      then forall p, P(p)
*)
