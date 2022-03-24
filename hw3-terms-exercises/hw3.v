(* Write your proof terms in place of underscores below ("_") *)

Variables A B C : Prop.

(* Exercise 1: Prove implication is transitive. What does this logical lemma correspond to in functional programming? *)
Check
  (fun ab =>
   fun bc =>
     fun x => bc (ab x))
: (A -> B) -> (B -> C) -> (A -> C).

(* Exercise 2: Prove conjunction is associative *)
Check
  (fun ab_c =>
     conj
       (proj1 (proj1 ab_c))
       (conj
          (proj2 (proj1 ab_c))
          (proj2 ab_c))
  )
: (A /\ B) /\ C -> A /\ (B /\ C).

(* Exercise 3: Prove disjunction distributes over conjunction: *)
Check
  (fun a_bc => 
    match a_bc with
    | or_introl a => conj
                       (or_introl a)
                       (or_introl a)
    | or_intror bc => conj
                        (or_intror (proj1 bc))
                        (or_intror (proj2 bc))
    end
  )
: A \/ (B /\ C) -> (A \/ B) /\ (A \/ C).

(* Exercise 4: Prove weak form of Peirce's law holds in intuitionistic logic *)
Check
  (fun abaab : ((((A -> B) -> A) -> A) -> B) =>
     abaab (fun aba : ((A -> B) -> A) =>
           aba (fun a : A =>
                  abaab (fun aba2: ((A -> B) -> A) =>
                           a))))
: ((((A -> B) -> A) -> A) -> B) -> B.

(* Exercise 5: We can always add double negation (but cannot drop it in general) *)
Check
  (fun a : A =>
   fun na : (A -> False) =>
     na a)
: A -> ~ ~ A.

(* Exercise 6: Although we can in some special cases like the following: *)
Check
  (fun nnna : (((A -> False) -> False) -> False) =>
     (fun a : A =>
        nnna (fun na : (A -> False) =>
                na a : False) : False) : (A -> False)
  )
: ~ ~ ~ A -> ~ A.

Print or_intror.
(* Exercise 7: Prove we cannot add the negation of the law of excluded middle and have a sound logic.
   Keep in mind that "~ A" means "A -> False" *)
Check
  ((fun n_aVna : ((A \/ (A -> False)) -> False) =>
      n_aVna (
          or_intror (fun a => n_aVna (or_introl a)) : (A \/ (A -> False))
             )
   )
    : ((A \/ (A -> False)) -> False) -> False)
: ~ ~ (A \/ ~ A).
