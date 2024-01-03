Fixpoint exp x (n : nat) :=
  match n with
  | 0 => 1
  | S k => x * exp x k
  end.

Require Import Lia.

Theorem exp_k: forall x m n, exp x (m + n) = exp x m * exp x n.
Proof.
  intros x.
  induction m.
  - intros n. simpl. apply plus_n_O.
  - intros n. simpl.
    rewrite IHm.
    lia.
Qed.