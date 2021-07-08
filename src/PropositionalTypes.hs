-- | Propositional types module
module PropositionalTypes where

-- | The 'Name' type represents the name of an 'Atom'.
type Name = String

-- | The 'Valuation' type encodes a propositional valuation.
type Valuation = [(Name, Bool)]

-- | The 'Formula' datatype encodes a propositional formula.
data Formula
  = -- | A constant Boolean constructor.
    Const Bool
  | -- | The propositional 'Atom' constructor.
    Atom Name
  | -- | The 'Not' (negation) unary connective constructor.
    Not Formula
  | -- | The 'And' (conjunction) binary connective constructor.
    And Formula Formula
  | -- | The 'Or' (disjunction) binary connective constructor.
    Or Formula Formula
  | -- | The 'Implies' (if then) binary connective constructor.
    Implies Formula Formula
  | -- | The 'Iff' (if and only if) binary connective constructor.
    Iff Formula Formula

-- | The 'KnowledgeBase' type encodes a propositional knowledge base.
type KnowledgeBase = [Formula]

instance Show Formula where
  show (Const True ) = "⊤"
  show (Const False) = "⊥"
  show (Atom  s    ) = show s
  show (Not   p    ) = "(" ++ "¬" ++ show p ++ ")"
  show (And     p q) = "(" ++ show p ++ "∧" ++ show q ++ ")"
  show (Or      p q) = "(" ++ show p ++ "∨" ++ show q ++ ")"
  show (Implies p q) = "(" ++ show p ++ "→" ++ show q ++ ")"
  show (Iff     p q) = "(" ++ show p ++ "↔" ++ show q ++ ")"

instance Eq Formula where
  (==) (Atom s1    ) (Atom s2    ) = s1 == s2
  (==) (Not  p     ) (Not  q     ) = p == q
  (==) (And     p q) (And     r s) = p == r && q == s || p == s && q == r
  (==) (Or      p q) (Or      r s) = p == r && q == s || p == s && q == r
  (==) (Implies p q) (Implies r s) = p == r && q == s
  (==) (Iff     p q) (Iff     r s) = p == r && q == s || p == s && q == r
  (==) _             _             = False
