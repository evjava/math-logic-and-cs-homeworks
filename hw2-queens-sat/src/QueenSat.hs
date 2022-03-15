
module QueenSat where

import Formula
import Normalizer
import Board
import FormulasUtils
import MinisatHelper
import BoardPrinter

-- FORMULA GENERATION LOGIC

makeLiteral :: Size -> Pos -> Literal
makeLiteral size (x, y) = A $ V $ show (y * size + x + 1)

makeAllLiterals :: Size -> [[Pos]] -> [[Literal]]
makeAllLiterals size posLists = map (map $ makeLiteral size) posLists

formulaQueens :: Size -> Formula
formulaQueens size =
  let
    stright = makeAllLiterals size $ genStrightLines size
    strightF = map formulaSumEq1 stright
    diag = makeAllLiterals size $ genDiagLines size
    diagF = map formulaSumLeq1 diag
    formulas = strightF ++ diagF
  in formulaAnd formulas

trimTrivialClauses :: CNF -> CNF
trimTrivialClauses (CNFList clauses) = CNFList $ filter (not . elem (A T)) clauses


main :: IO ()
main = do
  putStrLn "Enter size:"
  input <- getLine
  let size = (read input :: Int)
  sol <- runMinisat $ trimTrivialClauses $ conv_f_cnf $ formulaQueens size
  putStrLn $ showBoard size sol
