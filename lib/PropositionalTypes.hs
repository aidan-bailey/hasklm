-- | Propositional types module
module PropositionalTypes where

-- | Name type
type Name = String

-- | Valuation valuation type
type Valuation = [(Name, Bool)]

-- | type for formula of logic
data Formula
  = -- | Const
    Const Bool
  | -- | Atom
    Atom Name
  | -- | Not
    Not Formula
  | -- | And
    And Formula Formula
  | -- | Or
    Or Formula Formula
  | -- | Implication
    Implies Formula Formula
  | -- | If and only if
    Iff Formula Formula

-- | KnowledgeBase type
type KnowledgeBase = [Formula]

-- | Show instance of form
instance Show Formula where
  show (Const b    ) = show b
  show (Atom  s    ) = show s
  show (Not   p    ) = "¬" ++ show p
  show (And     p q) = show p ++ "∧" ++ show q
  show (Or      p q) = show p ++ "∨" ++ show q
  show (Implies p q) = show p ++ "→" ++ show q
  show (Iff     p q) = show p ++ "↔" ++ show q

{- Potentially useful, not sure at the moment
-- | Eq instance of form
instance Eq Formula where
  (==) (Atom s1    ) (Atom s2    ) = s1 == s2
  (==) (Not  p     ) (Not  q     ) = p == q
  (==) (And     p q) (And     r s) = p == r && q == s || p == s && q == r
  (==) (Or      p q) (Or      r s) = p == r && q == s || p == s && q == r
  (==) (Implies p q) (Implies r s) = p == r && q == s
  (==) (Iff     p q) (Iff     r s) = p == r && q == s || p == s && q == r
  (==) _             _             = False
-}
