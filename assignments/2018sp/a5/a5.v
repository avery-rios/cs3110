(* You may modify these imports, but you shouldn't need to. *)
Require Import List Arith Bool.
Import ListNotations.
Require Import Lia.

(** * A5 *)

(********************************************************************


               AAA            555555555555555555
              A:::A           5::::::::::::::::5
             A:::::A          5::::::::::::::::5
            A:::::::A         5:::::555555555555
           A:::::::::A        5:::::5
          A:::::A:::::A       5:::::5
         A:::::A A:::::A      5:::::5555555555
        A:::::A   A:::::A     5:::::::::::::::5
       A:::::A     A:::::A    555555555555:::::5
      A:::::AAAAAAAAA:::::A               5:::::5
     A:::::::::::::::::::::A              5:::::5
    A:::::AAAAAAAAAAAAA:::::A 5555555     5:::::5
   A:::::A             A:::::A5::::::55555::::::5
  A:::::A               A:::::A55:::::::::::::55
 A:::::A                 A:::::A 55:::::::::55
AAAAAAA                   AAAAAAA  555555555

*********************************************************************)

(**

Here is an OCaml interface for queues:
<<
(* ['a t] is a queue containing values of type ['a]. *)
type 'a t

(* [empty] is the empty queue *)
val empty : 'a t

(* [is_empty q] is whether [q] is empty *)
val is_empty : 'a t -> bool

(* [front q] is [Some x], where [x] the front element of [q],
 * or [None] if [q] is empty. *)
val front : 'a t -> 'a option

(* [enq x q] is the queue that is the same as [q], but with [x]
 * enqueued (i.e., inserted) at the end. *)
val enq : 'a -> 'a t -> 'a t

(* [deq x q] is the queue that is the same as [q], but with its
 * front element dequeued (i.e., removed).  If [q] is empty,
 * [deq q] is also empty. *)
val deq : 'a t -> 'a t
>>

Note that the specification for [deq] differs from what we have given
before:  the [deq] of an empty list is the empty list; there are no
options or exceptions involved.

Here is an equational specification for that interface:
<<
(1) is_empty empty      = true
(2) is_empty (enq _ _ ) = false
(3) front empty         = None
(4) front (enq x q)     = Some x         if is_empty q = true
(5) front (enq _ q)     = front q        if is_empty q = false
(6) deq empty           = empty
(7) deq (enq _ q)       = empty          if is_empty q = true
(8) deq (enq x q)       = enq x (deq q)  if is_empty q = false
>>

Your task in the next two parts of this file is to implement the Coq equivalent
of that interface and prove that your implementation satisfies the equational
specification.  Actually, you will do this twice, with two different
representation types, as we studied back in Lab 7:

- _simple queues_, which represent a queue as a singly-linked list
  and have worst-case linear time performance.

- _two-list queues_, which represent a queue with two singly-linked
  lists, and have amortized constant time performance.

*)




(********************************************************************)
(** ** Part 1: Simple Queues *)
(********************************************************************)

Module SimpleQueue.

(** Your first task is to implement and verify simple queues.
    To get you started, we provide the following definitions for
    the representation type of a simple queue, and for the empty
    simple queue. *)

(** [queue A] is the type that represents a queue as a
    singly-linked list.  The list [[x1; x2; ...; xn]] represents
    the queue with [x1] at its front, then [x2], ..., and finally
    [xn] at its end.  The list [[]] represents the empty queue. *)
Definition queue (A : Type) := list A.

Definition empty {A : Type} : queue A := [].

(**
*** Implementation of simple queues.
Define [is_empty], [front], [enq], and [deq]. We have provided some starter code
below that type checks, but it defines those operations in trivial and incorrect
ways. _Hint:_ this isn't meant to be tricky; you just need to translate the code
you would naturally write in OCaml into Coq syntax.
*)

Definition is_empty {A : Type} (q : queue A) : bool :=
  match q with
  | [] => true
  | _ => false
  end.

Definition front {A : Type} (q : queue A) : option A :=
  match q with
  | h :: _ => Some h
  | [] => None
  end.

Fixpoint enq {A : Type} (x : A) (q : queue A) : queue A :=
  match q with
  | [] => [x]
  | h :: t => h :: enq x t
  end.

Definition deq {A : Type} (q : queue A) : queue A :=
  match q with
  | [] => []
  | _ :: t => t
  end.

(**
*** Verification of simple queues.
Prove that the equations in the queue specification hold. We have written
them for you, below, but instead of a proof we have written [Admitted].  That
tells Coq to accept the theorem as true (hence it will compile) even though
there is no proof.  You need to replace [Admitted] with [Proof. ...  Qed.]
_Hint:_ none of these proofs requires induction.
*)

Theorem eqn1 : forall (A : Type),
  is_empty (@empty A) = true.
Proof. reflexivity. Qed.

Theorem eqn2 : forall (A : Type) (x : A) (q : queue A),
  is_empty (enq x q) = false.
Proof.
  intros A x. destruct q.
  - reflexivity.
  - reflexivity.
Qed.

Theorem eqn3 : forall (A : Type),
  front (@empty A) = None.
Proof. reflexivity. Qed.

Theorem eqn4 : forall (A : Type) (x : A) (q : queue A),
  is_empty q = true -> front (enq x q) = Some x.
Proof.
  intros A x. destruct q.
  - intros He. reflexivity.
  - simpl. intros He. discriminate He.
Qed.

Theorem eqn5 : forall (A : Type) (x : A) (q : queue A),
  is_empty q = false -> front (enq x q) = front q.
Proof.
  intros A x. destruct q.
  - simpl. intros He. discriminate He.
  - intros _. reflexivity.
Qed.

Theorem eqn6 : forall (A : Type),
  deq (@empty A) = (@empty A).
Proof. reflexivity. Qed.

Theorem eqn7 : forall (A : Type) (x : A) (q : queue A),
  is_empty q = true -> deq (enq x q) = empty.
Proof.
  intros A x. destruct q.
  - intros _. reflexivity.
  - simpl. intros He. discriminate He.
Qed.

Theorem eqn8 : forall (A : Type) (x : A) (q : queue A),
  is_empty q = false -> deq (enq x q) = enq x (deq q).
Proof.
  intros A x. destruct q.
  - simpl. intros He. discriminate He.
  - intros _. reflexivity.
Qed.

End SimpleQueue.





(********************************************************************)
(** ** Part 2: Two-list Queues *)
(********************************************************************)

Module TwoListQueue.

(** Your second task is to implement and verify two-list queues.
    To get you started, we provide the following definitions for
    the representation type of a two-list queue, and for the empty
    two-list queue. *)

(** [queue A] is the type that represents a queue as a pair of two
    singly-linked lists.  The pair [(f,b)] represents the same
    queue as does the simple queue [f ++ rev b].  The list [f] is
    the front of the queue, and the list [b] is the back of the
    queue stored in reversed order.

    _Representation invariant:_  if [f] is [nil] then [b] is [nil].

    The syntax [% type] in this definition tells Coq to treat the
    [*] symbol as the pair constructor rather than multiplication.
    You shouldn't need to use that syntax anywhere in your solution. *)
Definition queue (A : Type) := (list A * list A) % type.

(** [rep_ok q] holds iff [q] satisfies its RI as stated above *)
Definition rep_ok {A : Type} (q : queue A) : Prop :=
  match q with
  | (f,b) => f = [] -> b = []
  end.

Definition empty {A : Type} : queue A := ([],[]).

(**
*** Implementation of two-list queues.
Define [is_empty], [front], [enq], and [deq]. We have provided some starter code
below that type checks, but it defines those operations in trivial and incorrect
ways. _Hint:_ this isn't meant to be tricky; you just need to translate the code
you would naturally write in OCaml into Coq syntax.  You will need to define
one new function as part of that.
*)

Definition is_empty {A : Type} (q : queue A) : bool :=
  match q with
  | ([], _) => true
  | _ => false
  end.

Definition front {A : Type} (q : queue A) : option A :=
  match q with
  | (h :: _, _) => Some h
  | _ => None
  end.

Definition enq {A : Type} (x : A) (q : queue A) : queue A :=
  match q with
  | ([], _) => ([x], [])
  | (f, b) => (f, x :: b)
  end.

Definition deq {A : Type} (q : queue A) : queue A :=
  match q with
  | ([], _) => empty
  | ([_], b) => (rev b, [])
  | (h :: t, b) => (t, b)
  end.

(**
*** Verification of two-list queues.
Next you need to prove that the equations in the queue specification hold.
The statements of those equations below now include as a precondition
that the RI holds of any input queues.
_Hint:_ none of these proofs requires induction, but they will be
harder and longer than the simple queue proofs.
*)

Theorem eqn1 : forall (A : Type),
  is_empty (@empty A) = true.
Proof. reflexivity. Qed.

Theorem eqn2 : forall (A : Type) (x : A) (q : queue A),
  rep_ok q -> is_empty (enq x q) = false.
Proof.
  intros A x [f b]. destruct f.
  - reflexivity.
  - reflexivity.
Qed.

Theorem eqn3 : forall (A : Type),
  front (@empty A) = None.
Proof. reflexivity. Qed.

Theorem eqn4 : forall (A : Type) (x : A) (q : queue A),
  rep_ok q -> is_empty q = true -> front (enq x q) = Some x.
Proof.
  intros A x [f b]. destruct f.
  - reflexivity.
  - simpl. intros _ Hc. discriminate Hc.
Qed.

Theorem eqn5 : forall (A : Type) (x : A) (q : queue A),
  rep_ok q -> is_empty q = false -> front (enq x q) = front q.
Proof.
  intros A x [f b]. destruct f.
  - simpl. intros _ Hc. discriminate Hc.
  - reflexivity.
Qed.

Theorem eqn6 : forall (A : Type),
  deq (@empty A) = @empty A.
Proof. reflexivity. Qed.

Theorem eqn7 : forall (A : Type) (x : A) (q : queue A),
  rep_ok q -> is_empty q = true -> deq (enq x q) = empty.
Proof.
  intros A x [f b]. destruct f.
  - reflexivity.
  - simpl. intros _ Hc. discriminate Hc.
Qed.

(**
It turns out that two-list queues actually do not satisfy [eqn8]! To show that,
find a counterexample:  values for [x] and [q] that cause [eqn8] to be invalid.
Plug in your values for [x] and [q] below, then prove the three theorems
[counter1], [counter2], and [counter3].  _Hint_: if you choose your values well,
the proofs should be easy; each one should need only about one tactic.
*)

Module CounterEx.

Definition x : nat := 0.
(* change [0] to a value of your choice *)
Definition q : (list nat * list nat) := ([x], [x]).
(* change [empty] to a value of your choice *)

Theorem counter1 : rep_ok q.
Proof.
  simpl. intros H. apply H.
Qed.

Theorem counter2 : is_empty q = false.
Proof. reflexivity. Qed.

Theorem counter3 : deq (enq x q) <> enq x (deq q).
Proof. simpl. discriminate. Qed.

End CounterEx.

(**
Two-list queues do satisfy a relaxed version of [eqn8], though,
where instead of requiring [deq (enq x q)] and [enq x (deq q)]
to be _equal_, we only require them to be _equivalent_ after being
converted to simple queues.  The following definition implements
that idea of equivalence:
*)

Definition equiv {A:Type} (q1 q2 : queue A) : Prop :=
  match (q1, q2) with
  | ((f1,b1),(f2,b2)) => f1 ++ rev b1 = f2 ++ rev b2
  end.

(* Hint Unfold equiv. *)
(* The command above gives a hint to the [auto] tactic to try unfolding
   [equiv] as part of its proof search.  This will help you in the
   next proof. *)

(**
Now prove that the following relaxed form of [eqn8] holds.  _Hint:_
this is probably the hardest proof in the assignment.  Don't hesitate
to manage the complexity of the proof by stating and proving helper lemmas.
*)

Theorem eqn8_equiv : forall (A : Type) (x : A) (q : queue A),
  rep_ok q -> is_empty q = false ->
  equiv (deq (enq x q)) (enq x (deq q)).
Proof.
  intros A x [f b].
  destruct f.
  - simpl. intros _ Hc. discriminate Hc.
  - intros _ _. destruct f.
    + simpl. destruct (rev b).
      * reflexivity.
      * unfold equiv. simpl. rewrite app_nil_r. reflexivity.
    + simpl. reflexivity.
Qed.

(**
Finally, verify that [empty] satisfies the RI, and that [enq] and [deq] both
preserve the RI.
*)

Theorem rep_ok_empty : forall (A : Type),
  rep_ok (@empty A).
Proof. intros A H. apply H. Qed.

Theorem rep_ok_enq : forall (A : Type) (q : queue A),
  rep_ok q -> forall (x : A), rep_ok (enq x q).
Proof.
  intros A [f b] _ x. destruct f.
  - intros _. reflexivity.
  - simpl. intros Hc. discriminate Hc.
Qed.

Theorem rep_ok_deq: forall (A : Type) (q : queue A),
  rep_ok q -> rep_ok (deq q).
Proof.
  intros A [f b] _. destruct f.
  - intros _. reflexivity.
  - destruct f.
    + simpl. intros _. reflexivity.
    + simpl. intros Hc. discriminate Hc.
Qed.

End TwoListQueue.

(********************************************************************)
(** ** Part 3: Logic *)
(********************************************************************)

Module Logic.

(**
Prove each of the following theorems.  You may _not_ use the [tauto]
or [auto] tactic for the proofs in this module.
*)

Theorem logic1 : forall P Q R S: Prop,
  (P /\ Q) -> (R /\ S) -> (Q /\ R).
Proof.
  intros P Q R S [HP HQ] [HR HS]. split.
  - apply HQ.
  - apply HR.
Qed.

Theorem logic2 : forall P Q R S : Prop,
  (P -> Q) -> (R -> S) -> (P \/ R) -> (Q \/ S).
Proof.
  intros P Q R S HPQ HRS HPR.
  destruct HPR.
  - left. apply HPQ. apply H.
  - right. apply HRS. apply H.
Qed.

Theorem logic3 : forall P : Prop,
  P -> ~~P.
Proof.
  intros P HP contra. apply contra. apply HP.
Qed.

Theorem logic4 : forall P Q : Prop,
  (P -> Q) -> (~~P -> ~~Q).
Proof.
  intros P Q HPQ Hnp Hnq.
  apply Hnp. intros HP. apply Hnq. apply HPQ. apply HP.
Qed.

End Logic.

(********************************************************************)
(** ** Part 4: Induction *)
(********************************************************************)

Module Induction.

(**
Here is an OCaml function:
<<
let rec sum_cubes_to n =
  if n = 0 then 0
  else n*n*n + sum_cubes_to (n-1)
>>

Prove that
<<
  sum_cubes_to n  =  n * n * (n+1) * (n+1) / 4
>>

First, prove it mathematically (i.e., not in Coq), by completing
the following template.

-----------------------------------------------------------------
Theorem:  sum_cubes_to n = n * n * (n+1) * (n+1) / 4.

Proof:  by induction on n.

FILL IN your proof here.  You must state the property P, the base case,
the inductive case, and the inductive hypothesis, and justify the
reasoning you use in each proof step.  "By algebra" is an acceptable
justification for collecting terms, adding, multiplying, etc.  If you
need to judiciously break the 80-column limit, that's okay.

QED.
-----------------------------------------------------------------

Second, prove it in Coq, by completing the following code.
*)

(** [sum_cubes_to n] is [0*0*0 + 1*1*1 + ... + n*n*n]. *)
Fixpoint sum_cubes_to (n:nat) : nat :=
  match n with
  | O => O
  | S k => n * n * n + sum_cubes_to k
  end.


Theorem sum_cubes : forall n,
  4 * sum_cubes_to n = n * n * (n+1) * (n+1).
Proof.
  induction n.
  - reflexivity.
  - cbn delta fix. cbn iota. lia.
Qed.

End Induction.

(********************************************************************)
(** THE END *)
(********************************************************************)
