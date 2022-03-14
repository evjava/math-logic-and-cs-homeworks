{-# Language FlexibleInstances #-}

import Data.List (nub, sort, intercalate)
-- import Test.QuickCheck

import Normalizer
import Evaluator

---- var extractor
class VarExtractable a where
  vars :: a -> [String]

instance VarExtractable Atom where
  vars (V v) = [v]
  vars _ = []

instance VarExtractable Literal where
  vars (A a) = vars a
  vars (N a) = vars a

instance VarExtractable Formula where
  vars (L l) = vars l
  vars (Not f) = vars f
  vars (Binop f _ g) = sort $ nub $ (vars f) ++ (vars g)

instance VarExtractable [Literal] where
  vars ls = sort $ nub $ concat $ map vars ls

instance VarExtractable [[Literal]] where
  vars lss = sort $ nub $ concat $ map vars lss

instance VarExtractable DNF where
  vars (DNFList lss) = vars lss

instance VarExtractable CNF where
  vars (CNFList lss) = vars lss

---- generating contexts
allContexts :: [String] -> [Context]
allContexts [] = [[]]
allContexts (v:vs) = [(v, False):s | s <- suba] ++ [(v, True):s | s <- suba]
  where suba = allContexts vs

-- tests
formulaInfo :: Formula -> String
formulaInfo f = intercalate "\n\t"
  [ "Formula:"
  , infoF "  f" f
  , infoF "nnf" nnf
  , infoF "cnf" cnf
  , infoF "dnf" dnf
  ]
  where
    nnf = conv_f_nnf f
    dnf = conv_f_dnf f
    cnf = conv_f_cnf f
    infoF :: Show e => String -> e -> String
    infoF msg f = msg ++ ": " ++ (show f)


errorInfo :: Formula -> NNF -> DNF -> CNF -> Context -> String
errorInfo f nnf dnf cnf ctx = intercalate "\n\t"
  [ "Failed on formula with ctx: " ++ (show ctx)
  , infoEvaluatable "  f" f
  , infoEvaluatable "nnf" nnf
  , infoEvaluatable "cnf" cnf
  , infoEvaluatable "dnf" dnf
  ]
  where
    infoEvaluatable :: (Evaluatable e, Show e) => String -> e -> String
    infoEvaluatable msg e = msg ++ ": " ++ (show e) ++ "-> " ++ (show (eval ctx e))

check :: Formula -> NNF -> DNF -> CNF -> Context -> Bool
check f nnf dnf cnf ctx = if correct then True
                          else error (errorInfo f nnf dnf cnf ctx)
  where f_res = eval ctx f
        nnf_res = eval ctx nnf
        dnf_res = eval ctx dnf
        cnf_res = eval ctx cnf
        correct = all (== f_res) $ [nnf_res, dnf_res, cnf_res]

checkAll :: Formula -> Bool
checkAll f = all (check f nnf dnf cnf) ctxs
  where vs = vars f
        ctxs = allContexts vs
        nnf = conv_f_nnf f
        dnf = conv_f_dnf f
        cnf = conv_f_cnf f

f1 :: Formula
f1 = Binop (L (A (V "c"))) Or (Binop (L (A (V "a"))) Implies (L (N (V "b"))))

f2 :: Formula
f2 = neg $ f1

f3 :: Formula
f3 = (Binop
       (Binop
        (Binop (L (A (V "x1"))) Or (L (A (V "y1"))))
        And
        (Binop (L (A (V "x2"))) Or (L (A (V "y2")))))
       And
       (Binop (L (A (V "x3"))) Or (L (A (V "y3")))))

f4 :: Formula
f4 = (Binop
       (Binop
        (Binop (L (A (V "x1"))) And (L (A (V "y1"))))
        Or
        (Binop (L (A (V "x2"))) And (L (A (V "y2")))))
       Or
       (Binop (L (A (V "x3"))) And (L (A (V "y3")))))

f5 :: Formula
f5 = Binop (L (A (V "c"))) And (Binop (L (A (V "a"))) Iff (L (N (V "b"))))

main :: IO ()
main = do
  putStrLn "\n--------------------"
  let formulas = [f1, f2, f3, f4, f5]
  mapM_ putStrLn $ map formulaInfo $ formulas
  putStrLn $ if (all checkAll formulas) then "Status: Passed!"
             else "Status: Failed!"
  putStrLn "--------------------\n"

