Fixpoint fib0 b0 b1 (n : nat) :=
  match n with
  | 0 => b0
  | 1 => b1
  | S (S k2 as k1) => fib0 b0 b1 k1 + fib0 b0 b1 k2
  end.

Example fib0_4: fib0 0 1 4 = 3.
Proof. reflexivity. Qed.

Definition fib_ind  (P : nat -> Prop) (p0 : P 0) (p1 : P 1) 
  (IHp : forall n, P n -> P (S n) -> P (S (S n))) :
  forall n, P n := 
  let fix ind_conj n : (P n /\ P (S n)) :=
    match n with
    | 0 => conj p0 p1
    | S k =>
        match ind_conj k with
        | conj pk0 pk1 => conj pk1 (IHp k pk0 pk1)
        end
    end
   in 
   fun n =>
    match ind_conj n with
    | conj p _ => p
    end.

Require Import Coq.micromega.Lia.

Lemma fib0_base: forall n b0 b1, fib0 b0 b1 (S n) = fib0 b1 (b0 + b1) n.
Proof.
  induction n using fib_ind ; intros b0 b1.
  - reflexivity.
  - simpl. lia.
  - cbn delta fix. cbn iota.
    rewrite <- IHn0. rewrite <- IHn.
    reflexivity.
Qed.

Definition fib (n : nat) := fib0 0 1 n.

Fixpoint fibi (n : nat) prev curr :=
  match n with
  | 0 => prev
  | 1 => curr
  | S k => fibi k curr (prev + curr)
  end.

Example fibi_2: fibi 2 0 1 = 1.
Proof. reflexivity. Qed.

Lemma fibi_eq_fib0 : forall n prev curr, fibi n prev curr = fib0 prev curr n.
Proof.
  induction n using fib_ind; intros prev curr.
  - reflexivity.
  - reflexivity.
  - cbn delta fix. cbn iota.
    rewrite IHn0. rewrite <- fib0_base.
    reflexivity.
Qed.

Theorem fibi_eq_fib : forall n, fibi n 0 1 = fib n.
Proof.
  intros n.
  rewrite fibi_eq_fib0.
  reflexivity.
Qed.
