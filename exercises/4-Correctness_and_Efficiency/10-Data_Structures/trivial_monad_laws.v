Inductive t (T:Type) := wrap (v : T) : t T.

Definition m_return {T:Type} (r:T) : t T := wrap T r.

Definition bind {T U:Type} (r: t T) (f : T -> t U) : t U :=
  match r with 
  | wrap _ x => f x
  end.

Definition bind_comp {T U V : Type} (f : T -> t U) (g : U -> t V) : T -> t V :=
  fun x => bind (f x) g.

Theorem id_l : forall {T U : Type} (f : T -> t U) (x : T),
  bind (m_return x) f = f x.
Proof.
  intros T U f x.
  reflexivity.
Qed.

Theorem id_r : forall {T : Type} (m : t T), bind m m_return = m.
Proof.
  intros T m.
  destruct m. reflexivity.
Qed.

Theorem bind_assoc : forall {T U V W : Type}
  (f : T -> t U) (g : U -> t V) (h : V -> t W) (x : T),
  (bind_comp (bind_comp f g) h) x = (bind_comp f (bind_comp g h)) x.
Proof.
  intros T U V W f g h x.
  unfold bind_comp.
  destruct (f x). simpl.
  destruct (g v). simpl.
  reflexivity.
Qed.
