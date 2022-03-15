
module MinisatHelper where

import Formula
import Normalizer
import VarExtractor

import Data.List.Split ( splitOn );
import Data.List ( delete );
import System.Process;

cnfToDIMACS :: CNF -> String
cnfToDIMACS cnf =
  let
    (CNFList cnfl) = cnf
    varsCount = length $ vars cnf
    header = "p cnf " ++ (show varsCount) ++ " " ++ (show $ length cnfl) ++ "\n"
    showClause :: [Literal] -> String
    showClause cl = (foldl1 (\a b -> a ++ " " ++ b) (map (repl_negs . show) cl)) ++ " 0"
      where repl_negs :: String -> String
            repl_negs s = map (\c -> if c == '¬' then '-'; else c) s

  in header ++ (unlines $ map showClause cnfl)


runMinisat :: CNF -> IO [Int]
runMinisat cnf = do
  let dimacs = cnfToDIMACS cnf
  writeFile "queen_sat.cnf" dimacs
  _ <- system "minisat queen_sat.cnf queen_sat.out"
  vs <- processMinisatOut <$> readFile "queen_sat.out"
  return vs

processMinisatOut :: String -> [Int]
processMinisatOut f | head (lines f) /= "SAT" = []
                    | otherwise = vs
  where vs = delete 0 $ map (read :: String -> Int) $ splitOn " " $ (lines f !! 1)
