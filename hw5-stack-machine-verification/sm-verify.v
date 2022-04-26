From Coq Require Import List Arith.
Import ListNotations.

(* Arithmetic expressions language *)
Inductive expr : Type :=
| Const : nat -> expr
| Plus : expr -> expr -> expr
| Minus : expr -> expr -> expr.

(* Semantics of arithmetic expressions *)
Fixpoint eval (e : expr) : nat :=
  match e with
  | Const n => n
  | Plus e1 e2 => eval e1 + eval e2
  | Minus e1 e2 => eval e1 - eval e2
  end.

(* Stack machine instructions *)
Inductive instr :=
| Push : nat -> instr
| Add
| Sub.

(* Stack programs *)
Definition prog := list instr.

(* Stack *)
Definition stack := list nat.

(* Stack machine semantics *)
Fixpoint run (p : prog) (s : stack) {struct p}: stack :=
  match p with
  | [] => s
  | i :: p' =>
    match i with
    | Push n => run p' (n :: s)
    | Add =>
      match s with
      | a :: b :: s' => run p' (b + a :: s')
      | _ => [] (* if stack underflow -- interrupt
                   execution and return empty stack *)
      end
    | Sub =>
      match s with
      | a :: b :: s' => run p' (b - a :: s')
      | _ => [] (* if stack underflow -- interrupt
                    execution and return empty stack *)
      end
    end
  end.

(* Compilation from arithmetic expressions
   into stack programs *)
Fixpoint compile (e : expr) : prog :=
  match e with
  | Const n => [Push n]
  | Plus e1 e2 =>
    compile e1 ++ compile e2 ++ [Add]
  | Minus e1 e2 =>
    compile e1 ++ compile e2 ++ [Sub]
  end.

Lemma run_cat (p1 : prog) (p2 : prog) (s : stack) :
  run p1 s <> [] ->
  run (p1 ++ p2) s = run p2 (run p1 s).
Proof.
  revert s; induction p1 as [|i1 p1 IHp1]; intros s.
  - simpl. reflexivity.
  - destruct i1; simpl.
    + apply IHp1.
    + destruct s as [uu aa |top1 s].
      * contradiction.
      * destruct s as [| top2 s].
        ** contradiction.
        ** apply IHp1.
    + destruct s as [uu aa |top1 s].
      * contradiction.
      * destruct s as [| top2 s].
        ** contradiction.
        ** apply IHp1.
Qed.

Lemma Add_spec a b s :
  run [Add] (a::b::s) = (b + a)::s.
Proof.
  revert a b. simpl. reflexivity.
Qed.

Lemma Sub_spec a b s :
  run [Sub] (a::b::s) = (b - a)::s.
Proof.
  revert a b. simpl. reflexivity.
Qed.

(*  
Lemma run_Add :
  forall a b s, length(run [Add] (a::b::s)) = S ( length(s) ).
Proof.
  intros a b s.
  
    
    apply O_S.
    intros (length s').
    Search (0 = S _).



destruct (length []).
    destruct (length s').
    + reflexivity.
*)      
    


(* нужно переформулировать, что стек растёт *)

Lemma run_compile_non_empty :
  forall e s, exists x, run (compile e) s = x::s.
Proof.
  induction e as [| e1 IHp1 e2 IHp2 |]; intros s; simpl.
  - exists n. reflexivity.
  - rewrite !run_cat.
    + destruct (run (compile e1) s) eqn:E.
      assert (H: run (compile e1) s = []).
      rewrite E.


generalize (IHp1 s). 

      
      
      
      destruct (run (compile e2) (n :: s0)) eqn:E2.
      generalize (IHp2 (n :: s0)). rewrite E2. discriminate.
      
      rewrite !Add_spec.
        
        

      
  induction e; intros s; simpl.
  - discriminate. (* const *)
  - rewrite !run_cat.
    + simpl.
      (* сохраняем связь между E чёт там *)
      destruct (run (compile e2) (run (compile e1) s)) eqn:E.
      * congruence.
      * destruct s0.
Qed.

(*
Lemma compile_with_something :
  forall e,
    [eval e
*)
Theorem compile_correct :
  forall e,
    [eval e] = run (compile e) [].
Proof.
  intros e.
  induction e as [| e1 IHe1 e2 IHe2 | e1 IHe1 e2 IHe2].
  - reflexivity.
  - simpl.
    rewrite !run_cat.
    
      rewrite <- IHe1.

      
Qed.
