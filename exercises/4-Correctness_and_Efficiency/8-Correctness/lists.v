Require Import Coq.Lists.List.

Theorem append_nil : forall {A} (l : list A), l ++ nil = l.
Proof.
  intros A.
  induction l.
  - reflexivity.
  - simpl. rewrite IHl. reflexivity.
Qed.

Theorem rev_dist_append : forall {A} (l1 l2 : list A), rev (l1 ++ l2) = rev l2 ++ rev l1.
Proof.
  intros A.
  induction l1; intros l2.
  - simpl. symmetry. apply append_nil.
  - simpl. rewrite IHl1. symmetry. apply app_assoc.
Qed.

Theorem  rev_involutive : forall {A} (l : list A), rev (rev l) = l.
Proof.
  intros A.
  induction l.
  - reflexivity.
  - simpl. rewrite rev_dist_append.
    rewrite IHl. reflexivity.
Qed.