
module BoardPrinter where

import Board

import Data.List (intercalate)

s_queen :: Char
s_queen = '♛'
showBoard :: Size -> [Int] -> String
showBoard size sol =
  let s_sol :: [Char]
      s_sol = map (\x -> if (x < 0) then ' ' else s_queen) sol
      group :: [Char] -> [String]
      group li = case li of
                   [] -> []
                   _  -> (take size li):(group $ drop size li)
      ls = group s_sol
      pretty_ls = map prettify ls
        where prettify l = "│" ++ (intercalate "│" $ map (\x -> x:[]) l) ++ "│"
      sep_l = "├" ++ (intercalate "┼" $ (take size $ repeat "─")) ++ "┤"
      body  = intercalate ("\n" ++ sep_l ++ "\n") pretty_ls
      head' = "┌" ++ (intercalate "┬" $ (take size $ repeat "─")) ++ "┐"
      top   = "└" ++ (intercalate "┴" $ (take size $ repeat "─")) ++ "┘"
  in unlines [head', body, top]
