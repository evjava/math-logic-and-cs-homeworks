(* Write your proof terms in place of `provide_solution` *)
(* Do not use tactics! *)

Axiom provide_solution : forall {A}, A.

Section Logic.

(** Exercise: show that existential quantifier (the `ex` type) is a more general case of conjunction (the `and` type).
    This is because terms of type `ex` are dependent pairs, while terms of type `and`
    are non-dependent pairs, i.e. the type of the second component in independent of the
    value of the first one. *)

Definition and_via_ex (A B : Prop) :
  (exists (_ : A), B) <-> A /\ B
  :=
  conj
    (
      (fun xb =>
         match xb with
         | ex_intro _ x b => conj x b
         end
      ) :
      ((exists (_ : A), B) -> A /\ B)
    )
    (
      (fun ab =>
         match ab with
         | conj a b => ex_intro _ a b
         end
      ) :
      (A /\ B -> (exists (_ : A), B))
    ).

(* Print and_via_ex. *)

(** Exercise: The dual Frobenius rule *)

Definition Frobenius2 :=
  forall (A : Type) (P : A -> Prop) (Q : Prop),
    (forall x, Q \/ P x) <-> (Q \/ forall x, P x).

Definition LEM_iff_Frobenius2 :
  (forall P : Prop, P \/ ~ P) <-> Frobenius2
:= provide_solution.

End Logic.

Section ExtensionalEqualityAndComposition.

Variables A B C D : Type.

Notation "f \o g" := (fun x => f (g x)) (at level 50).
Notation "f =1 g" := (forall x, f x = g x) (at level 70, no associativity).

(** [=1] stands for extensional equality on unary functions *)

(** Exercise : associativity of function composition *)
Definition compA (f : A -> B) (g : B -> C) (h : C -> D) :
  (h \o g) \o f = h \o (g \o f)
:= eq_refl.

(** Exercise: Reflexivity *)
Definition eqext_refl :
  forall (f : A -> B), f =1 f
:= fun f => (fun x => eq_refl).

(** Exercise: Symmetry *)
Definition eqext_sym :
  forall (f g : A -> B), f =1 g -> g =1 f
  := fun f g =>
       (fun (eq_f_g : f =1 g) =>
          (fun (x : A) =>
             match
               (eq_f_g x : f x = g x) in (_ = gx)
               return (gx = f x)
             with
             | eq_refl => eq_refl (f x)
             end)).

(* reusing practice *)
Definition eq_trans A (x y z : A) :
  x = y -> y = z -> x = z
:=
  fun pf_xy : x = y =>
    match
      pf_xy in (_ = b)
      return (b = z -> x = z)
    with
    | eq_refl => fun (pf_xz : x = z) => pf_xz
    end.


(** Exercise: Transitivity *)
Definition eqext_trans :
  forall (f g h : A -> B), f =1 g -> g =1 h -> f =1 h
  := fun f g h eq_f_g eq_g_h (x : A) =>
          eq_trans B (f x) (g x) (h x) (eq_f_g x) (eq_g_h x).

(** Exercise: left congruence *)
Definition eq_compl :
  forall (f g : A -> B) (h : B -> C),
    f =1 g -> h \o f =1 h \o g
  := fun f g h eq_f_g (x : A) =>
       match (eq_f_g x : f x = g x) in (_ = gx)
             return (h (f x) = h gx)
       with
       | eq_refl => eq_refl (h (f x))
       end.

(** Exercise: right congruence *)
Definition eq_compr :
  forall (f g : B -> C) (h : A -> B),
    f =1 g -> f \o h =1 g \o h
  := fun f g h eq_f_g (x : A) =>
       eq_f_g (h x).

End ExtensionalEqualityAndComposition.
