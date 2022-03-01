{-# Language DeriveDataTypeable #-}

module Normalizer where

---- str constants
s_top :: String
s_top = "T"
s_bot :: String
s_bot = "⊥"
s_neg :: String
s_neg = " ¬ "
s_imp :: String
s_imp = " → "
s_and :: String
s_and = " ∧ "
s_or :: String
s_or  = " ∨ "
s_iff :: String
s_iff = " ↔ "

---- basic types
type Var = String

data Atom = V Var | T | B
instance Show Atom where
  show a = case a of
    (V var) -> var
    T       -> s_top
    B       -> s_bot

data Literal = A Atom | N Atom
instance Show Literal where
  show l = case l of
    (A a) -> (show a)
    (N a) -> "¬" ++ (show a)

class Negatable a where
  neg :: a -> a

instance Negatable Literal where
  neg x = case x of
            (A v) -> (N v)
            (N v) -> (A v)

data Op = Implies | And | Or | Iff deriving (Enum)
data Formula = L Literal
             | Not Formula -- negation
             | Binop Formula Op Formula

instance Show Formula where
  show f = case f of
    (L l)         -> (show l)
    (Not f')       -> s_neg ++ "(" ++ (show f') ++ ")"
    (Binop a o b) -> "(" ++ (show a) ++ so ++ (show b) ++ ")"
      where so = case o of
                   Implies -> s_imp
                   And -> s_and
                   Or -> s_or
                   Iff -> s_iff

instance Negatable Formula where
  neg f = case f of
    (Not f') -> f'
    _        -> (Not f)

---- NNF
data NOp = NAnd | NOr deriving (Show, Eq, Enum)
data NNF = NL Literal
         | NBinop NNF NOp NNF
instance Show NNF where
  show f = case f of
    (NL l) -> (show l)
    (NBinop a o b) -> "(" ++ (show a) ++ " " ++ so ++ " " ++ (show b) ++ ")"
      where so = case o of
                   NAnd -> "∧"
                   NOr -> "∨"

conv_f_nnf :: Formula -> NNF
conv_f_nnf f = let c     = conv_f_nnf
                   nc f' = (c $ neg f') in
  case f of
    (L l) -> (NL l)
    (Binop a o b) ->
      case o of
        Or      -> NBinop (c a) NOr (c b)
        And     -> NBinop (c a) NAnd (c b)
        Implies -> NBinop (nc a) NOr (c b)
        Iff     -> NBinop (NBinop na NAnd nb) NOr (NBinop neg_na NAnd neg_nb)
          where
            na = c a
            nb = c b
            neg_na = nc a
            neg_nb = nc b -- todo here is square?
    (Not f') ->
      case f' of
        (L l) -> NL $ neg l
        (Not f'') -> nc f''
        (Binop a o b) ->
          case o of
            Or -> NBinop (nc a) NAnd (nc b)
            And -> NBinop (nc a) NOr (nc b)
            Implies -> NBinop (c a) NAnd (nc b)
            Iff -> NBinop (NBinop na NAnd neg_nb) NOr (NBinop neg_na NAnd nb)
              where
                na = c a
                nb = c b
                neg_na = nc a
                neg_nb = nc b -- todo here is square?

---- DNF, CNF: common
s_join_c :: String -> [Literal] -> String
s_join_c _ [] = s_bot
s_join_c sep (x:xs@(_:_)) = (show x) ++ sep ++ (s_join_c sep xs)
s_join_c _ (x:[]) = (show x)

s_join :: String -> String -> [[Literal]] -> String
s_join sep_in sep_out lss =
  let joiner l = "(" ++ (s_join_c sep_in l) ++ ")" in
    case lss of
      [] -> s_bot
      (ls:t@(_:_)) -> (joiner ls) ++ sep_out ++ (s_join sep_in sep_out t)
      (ls:[]) -> (joiner ls)

conv_nnf_to :: NOp -> NNF -> [[Literal]]
conv_nnf_to _ (NL l) = [[l]]
conv_nnf_to sum_op (NBinop a o b) =
  let
    ca = conv_nnf_to sum_op a
    cb = conv_nnf_to sum_op b
  in
    if sum_op == o then ca ++ cb
    else                [x ++ y | x <- ca, y <- cb]

---- DNF
data DNF = DNFList [[Literal]]

instance Show DNF where
  show (DNFList lss) = s_join s_and s_or lss
  
conv_nnf_dnf :: NNF -> DNF
conv_nnf_dnf f = DNFList $ conv_nnf_to NOr f

conv_f_dnf :: Formula -> DNF
conv_f_dnf = conv_nnf_dnf . conv_f_nnf

---- CNF

data CNF = CNFList [[Literal]]

instance Show CNF where
  show (CNFList lss) = s_join s_or s_and lss
  
conv_nnf_cnf :: NNF -> CNF
conv_nnf_cnf f = CNFList $ conv_nnf_to NAnd f

conv_f_cnf :: Formula -> CNF
conv_f_cnf = conv_nnf_cnf . conv_f_nnf
