-- | Propositional helpers module
module PropositionalHelpers where

import           PropositionalTypes

---------------------
-- GENERAL HELPERS --
---------------------

-- | The 'int2bool' function takes in two ints and returns a Boolean list.
int2bool
  :: Int -- ^ The decimal to convert to binary.
  -> Int -- ^ The size of the list to be returned (padded with Falses).
  -> [Bool]
int2bool _ 0 = [False]
int2bool 0 i = False : int2bool 0 (i - 1)
int2bool n i = if even n
  then False : int2bool (div n 2) (i - 1)
  else True : int2bool (div n 2) (i - 1)

-- | The 'removeDuplicates' function returns the given list without duplicates.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates =
  foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- | The 'subValuation' function returns whether or not at least one of the given list of 'Valuation's is a subset of the given 'Valuation'.
subValuation :: [Valuation] -> Valuation -> Bool
subValuation [v] superval = and
  [ or [ (supN == subN) && (supB == subB) | (supN, supB) <- superval ]
  | (subN, subB) <- v
  ]
subValuation subvals superval = or [ subValuation [v] superval | v <- subvals ]

-- | The 'subsumes' function takes in two 'Valuation' lists and returns whether the first list subsumes the second.
subsumes :: [Valuation] -> [Valuation] -> Bool
subsumes subsumer subsumed = and [ subValuation subsumed v | v <- subsumer ]

-----------------
-- AUXILIARIES --
-----------------
