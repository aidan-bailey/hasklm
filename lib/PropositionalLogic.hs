-- | Propositional logic module
module PropositionalLogic where

import           PropositionalHelpers
import           PropositionalParser
import           PropositionalTypes

-- | The 'atoms' function returns an 'Atom' 'Name' list for the given `Formula`.
atoms :: Formula -> [Name]
atoms (Const _    ) = []
atoms (Atom  x    ) = [x]
atoms (Not   p    ) = atoms p
atoms (And     p q) = atoms p ++ atoms q
atoms (Or      p q) = atoms p ++ atoms q
atoms (Implies p q) = atoms p ++ atoms q
atoms (Iff     p q) = atoms p ++ atoms q

-- | The 'assigns' function returns the corresponding 'Atom' 's Boolean value for the given 'Valuation'.
assigns :: Valuation -> Name -> Bool
assigns [] _ = error "Atom not found in given valuation"
assigns ((a, b) : ve) n | n == a    = b
                        | otherwise = assigns ve n

-- | The 'satisfies' function evaluates a 'Formula' for the given 'Valuation'.
satisfies :: Valuation -> Formula -> Bool
satisfies _ (Const b    ) = b
satisfies v (Atom  n    ) = assigns v n
satisfies v (Not   p    ) = not (satisfies v p)
satisfies v (And     p q) = satisfies v p && satisfies v q
satisfies v (Or      p q) = satisfies v p || satisfies v q
satisfies v (Implies p q) = satisfies v p <= satisfies v q
satisfies v (Iff     p q) = satisfies v p == satisfies v q

-- | The 'valuations' function returns all possible 'Valuation's for the given 'KnowledgeBase'.
valuations :: KnowledgeBase -> [Valuation]
valuations kb = [ zip atomNames bools | bools <- boolPerms ]
 where
  atomNames       = removeDuplicates [ n | p <- kb, n <- atoms p ]
  valuationsCount = length atomNames ^ 2
  boolPerms =
    reverse [ int2bool i valuationsCount | i <- [0 .. valuationsCount - 1] ]

-- | The 'isValid' function returns the validity of the given 'Formula'.
isValid :: Formula -> Bool
isValid p = length (filter (`satisfies` p) valuationsList)
  == length valuationsList
  where valuationsList = valuations [p]

-- | The 'models' function returns the models (satisfying 'Valuation's) of the given `Formula`.
models :: KnowledgeBase -> [Valuation]
models [] = []
models kb = [ v | v <- valuations kb, and [ v `satisfies` p | p <- kb ] ]

-- | The 'entails' function returns whether or not the given 'KnowledgeBase' entails the given 'Formula'.
entails :: KnowledgeBase -> Formula -> Bool
entails [] p = isValid p
entails kb p = models kb `subsumes` models [p]

-- | The 'str2form' function returns the given String's 'Formula' representation.
str2form :: String -> Formula
str2form = parseString
