{-# Language FlexibleInstances #-}

module Evaluator where

import Formula
import Normalizer

import Data.Algebra.Boolean ((-->), (<-->))

type Context = [(String, Bool)]

class Evaluatable a where
  eval :: Context -> a -> Bool

instance Evaluatable String where
  eval [] _ = error "Value not found!"
  eval ((c,v):cs) x = if (c==x) then v
                      else           eval cs x

instance Evaluatable Atom where
  eval _ T = True
  eval _ B = False
  eval ctx (V v) = eval ctx v

instance Evaluatable Literal where
  eval ctx (A l) = eval ctx l
  eval ctx (N l) = not $ eval ctx l

instance Evaluatable Formula where
  eval ctx (L l) = eval ctx l
  eval ctx (Not f) = not $ eval ctx f
  eval ctx (Binop a o b) =
    let ca = eval ctx a
        cb = eval ctx b
    in case o of
         Implies -> ca -->  cb
         And     -> ca &&   cb
         Or      -> ca ||   cb
         Iff     -> ca <--> cb

instance Evaluatable NNF where
  eval ctx (NL l) = eval ctx l
  eval ctx (NBinop a o b) =
    let ca = eval ctx a
        cb = eval ctx b
    in case o of
         NAnd -> ca && cb
         NOr  -> ca || cb

type Combiner = [Bool] -> Bool
any' :: Combiner
any' = any id
all' :: Combiner
all' = all id

instance Evaluatable DNF where
  eval ctx (DNFList lss) = any' [ all' $ map (eval ctx) ls | ls <- lss]

instance Evaluatable CNF where
  eval ctx (CNFList lss) = all' [ any' $ map (eval ctx) ls | ls <- lss]
