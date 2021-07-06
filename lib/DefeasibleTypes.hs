-- | Defeasible logic types module
module DefeasibleTypes where

import PropositionalTypes

-- | Defeasible implication type
data DefeasibleImplication = Typically Formula Formula

-- | State type
type State = [Formula]

-- | Ranked models type
type RankedModels = [[State]]

-- | Show instance for a defeasible implication
instance Show DefeasibleImplication where
  show (Typically p q) = show p ++ "~>" ++ show q
