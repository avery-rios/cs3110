Fixpoint mult a b :=
  match a with
  | 0 => 0
  | S k => b + mult k b
  end.

Theorem mult_z : forall n, mult n 0 = 0.
Proof.
  induction n.
  - reflexivity.
  - simpl. apply IHn.
Qed.