module FormulasUtils where

import Formula

import ListUtils ( popAtAll )

formulaSumGeq1 :: [Literal] -> Formula
formulaSumGeq1 [] = L $ A B
formulaSumGeq1 (l:[]) = L l
formulaSumGeq1 (l:ls) = Binop (L l) Or (formulaSumGeq1 ls)

formulaSumEq0 :: [Literal] -> Formula
formulaSumEq0 [] = L $ A T
formulaSumEq0 (l:[]) = L $ neg l
formulaSumEq0 (l:ls) = Binop (L $ neg l) And (formulaSumEq0 ls)

formulaAnd :: [Formula] -> Formula
formulaAnd [] = L $ A T
formulaAnd (f:[]) = f
formulaAnd (f:fs) = Binop f And (formulaAnd fs)

formulaSumLeq1 :: [Literal] -> Formula
formulaSumLeq1 lits =
  let parts = popAtAll lits
      -- if l=1, then for all x in literals, x = 0
      f_parts = map ifLitOtherNot parts
        where
          ifLitOtherNot :: (Literal, [Literal]) -> Formula
          ifLitOtherNot (l, ls) = Binop (L $ l) Implies (formulaSumEq0 ls)
  in formulaAnd f_parts
        

formulaSumEq1 :: [Literal] -> Formula
formulaSumEq1 ls = Binop (formulaSumGeq1 ls) And (formulaSumLeq1 ls)
