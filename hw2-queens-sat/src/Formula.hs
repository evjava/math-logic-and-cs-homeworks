
module Formula where

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

data Atom = V Var | T | B deriving (Eq)
instance Show Atom where
  show a = case a of
    (V var) -> var
    T       -> s_top
    B       -> s_bot

data Literal = A Atom | N Atom deriving (Eq)
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
