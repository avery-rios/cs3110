Require Import Coq.Lists.List.

Section Fold2.

Variable (A : Type).
Variable (f : A -> A -> A).
Variable (HAssoc : forall x y z, f x (f y z) = f (f x y) z).

Lemma fold_left_acc : forall (l : list A) a b,
  fold_left f l (f a b) = f a (fold_left f l b).
Proof.
  induction l.
  - intros a b. reflexivity.
  - intros a0 b. simpl. rewrite <- IHl.
    f_equal. symmetry. apply HAssoc.
Qed.

Theorem fold_2 : forall (a : A),
  (forall x, f a x = x) ->
  (forall x, f x a = x) ->
  (forall (l : list A), fold_left f l a = fold_right f a l).
Proof.
  intros e HIdL HIdR.
  induction l.
  - reflexivity.
  - simpl. rewrite <- IHl.
    replace (f e a) with (f a e).
    + apply fold_left_acc.
    + rewrite HIdL. rewrite HIdR. reflexivity.
Qed.

End Fold2.

Example concat_lr : forall {A} (l : list (list A)),
  fold_left (@app A) l nil = fold_right (@app A) nil l.
Proof.
  intros A l.
  apply fold_2.
  - apply app_assoc.
  - intros x. reflexivity.
  - intros x. apply app_nil_r.
Qed.