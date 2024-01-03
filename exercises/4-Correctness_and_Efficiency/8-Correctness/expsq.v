Fixpoint exp x n :=
  match n with
  | 0 => 1
  | S k => x * exp x k
  end.

Require Import Lia.

Lemma exp_sq : forall n x, exp x (2 * n) = exp (x * x) n.
Proof.
  induction n; intros x.
  - reflexivity.
  - simpl. rewrite <- IHn. simpl.
    replace (n + S (n + 0)) with (S (n + (n + 0))).
    + simpl. lia.
    + lia.
Qed.

Require Import Coq.NArith.BinNat Coq.PArith.BinPos Coq.PArith.Pnat.

Fixpoint expsq_pos x n :=
  match n with
  | xH => x
  | xO k => expsq_pos (x * x) k
  | xI k => x * expsq_pos (x * x) k
  end.

Definition expsq_bin x n :=
  match n with
  | N0 => 1
  | Npos v => expsq_pos x v
  end.

Lemma exp_eq_expsq_pos : forall n x, exp x (Pos.to_nat n) = expsq_pos x n.
Proof.
  induction n; intros x.
  - rewrite Pos2Nat.inj_xI.
    cbn delta [exp expsq_pos] fix. cbn iota.
    rewrite <- IHn. f_equal.
    apply exp_sq.
  - rewrite Pos2Nat.inj_xO.
    cbn delta [exp expsq_pos] fix. cbn iota.
    rewrite <- IHn. apply exp_sq.
  - simpl. lia.
Qed.

Theorem exp_eq_expsq : forall n x, exp x (N.to_nat n) = expsq_bin x n.
Proof.
  intros n x.
  destruct n.
  - reflexivity.
  - simpl. apply exp_eq_expsq_pos.
Qed.