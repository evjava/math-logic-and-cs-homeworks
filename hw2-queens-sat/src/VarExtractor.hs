{-# Language FlexibleInstances #-}

module VarExtractor where

import Formula
import Normalizer

import Data.List (nub, sort)

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
