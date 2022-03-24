(* Write your proof terms in place of underscores below ("_") *)

Variables A B C : Prop.

(* Exercise 1: Prove implication is transitive. What does this logical lemma correspond to in functional programming? *)
Check
  (_)
: (A -> B) -> (B -> C) -> (A -> C).

(* Exercise 2: Prove conjunction is associative *)
Check
  (_)
: (A /\ B) /\ C -> A /\ (B /\ C).

(* Exercise 3: Prove disjunction distributes over conjunction: *)
Check
  (_)
: A \/ (B /\ C) -> (A \/ B) /\ (A \/ C).

(* Exercise 4: Prove weak form of Peirce's law holds in intuitionistic logic *)
Check
  (_)
: ((((A -> B) -> A) -> A) -> B) -> B.

(* Exercise 5: We can always add double negation (but cannot drop it in general) *)
Check
  (_)
: A -> ~ ~ A.

(* Exercise 6: Although we can in some special cases like the following: *)
Check
  (_)
: ~ ~ ~ A -> ~ A

(* Exercise 7: Prove we cannot add the negation of the law of excluded middle and have a sound logic.
   Keep in mind that "~ A" means "A -> False" *)
Check
  (_)
: ~ ~ (A \/ ~ A).
