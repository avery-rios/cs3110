Inductive tree (A : Type) : Type :=
  | Leaf : tree A
  | Node : tree A -> A -> tree A -> tree A.

Arguments Leaf {A}.
Arguments Node {A} l v r.

Fixpoint reflect {A} (t : tree A) : tree A :=
  match t with
  | Leaf => Leaf
  | Node l v r => Node (reflect r) v (reflect l)
  end.

Fixpoint size {A} (t : tree A) :=
  match t with
  | Leaf => 0
  | Node l v r => S (size l + size r)
  end.

Require Import Lia.

Theorem reflect_size : forall {A} (t : tree A), size (reflect t) = size t.
Proof.
  intros A.
  induction t.
  - reflexivity.
  - simpl. rewrite IHt1. rewrite IHt2. lia.
Qed.