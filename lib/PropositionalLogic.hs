-- | Propositional logic module
module PropositionalLogic where

import           PropositionalHelpers
import           PropositionalParser
import           PropositionalTypes

-- | atoms function outputs the atoms of a given formula
atoms :: Formula -> [Name]
atoms (Const _    ) = []
atoms (Atom  x    ) = [x]
atoms (Not   p    ) = atoms p
atoms (And     p q) = atoms p ++ atoms q
atoms (Or      p q) = atoms p ++ atoms q
atoms (Implies p q) = atoms p ++ atoms q
atoms (Iff     p q) = atoms p ++ atoms q

-- | assigns function searches for an atom's assigned value in a given valuation
assigns :: Valuation -> Name -> Bool
assigns [] _ = error "Atom not found in given valuation"
assigns ((a, b) : ve) n | n == a    = b
                        | otherwise = assigns ve n

-- | satisfies function evaluates a formula given a valuation
satisfies :: Valuation -> Formula -> Bool
satisfies _ (Const b    ) = b
satisfies v (Atom  n    ) = assigns v n
satisfies v (Not   p    ) = not (satisfies v p)
satisfies v (And     p q) = satisfies v p && satisfies v q
satisfies v (Or      p q) = satisfies v p || satisfies v q
satisfies v (Implies p q) = satisfies v p <= satisfies v q
satisfies v (Iff     p q) = satisfies v p == satisfies v q

-- | valuations function generates all possible valuations for a formula
valuations :: Formula -> [Valuation]
valuations p = [ zip atomNames bools | bools <- boolPerms ]
 where
  atomNames       = removeDuplicates (atoms p)
  valuationsCount = length atomNames * 2
  boolPerms =
    reverse [ int2bool i valuationsCount | i <- [0 .. valuationsCount] ]

-- | isValid function checks if a given formula is valid
isValid :: Formula -> Bool
isValid p = length (filter (`satisfies` p) valuationsList)
  == length valuationsList
  where valuationsList = valuations p

-- | models function returns the models for given formula
models :: KnowledgeBase -> [Valuation]
models [] = []
models kb =
  [ v | v <- valuations (head kb), and [ v `satisfies` p | p <- kb ] ]

-- | entails function checks if a knowledge base entails a formula
entails :: KnowledgeBase -> Formula -> Bool
entails [] p = isValid p
entails (q : kb) p | null kb   = [q] `entails` p
                   | otherwise = [q] `entails` p && kb `entails` p

-- | str2form function converts a string to a formula
str2form :: String -> Formula
str2form = parseString
