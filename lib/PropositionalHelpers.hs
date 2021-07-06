-- | Propositional helpers module
module PropositionalHelpers where

import           PropositionalTypes

{-
-- | int2bool function converts an integer to the corresponding binary (bool) value
int2bool :: Int -> Int -> [Bool]
int2bool _ 0         = [False]
int2bool 0 j | j > 0 = False : int2bool 0 (j - 1)
int2bool i j | i == 0    = False : int2bool i (j - 1)
             | otherwise = b : int2bool fval (j - 1)
 where
  fval     = i `div` 2
  leftOver = i `mod` 2
  b        = leftOver > 0
-}

int2bool :: Int -> Int -> [Bool]
int2bool _ 0 = [False]
int2bool 0 i = False : int2bool 0 (i - 1)
int2bool n i = if (mod n 2 == 0)
  then False : int2bool (div n 2) (i - 1)
  else True : int2bool (div n 2) (i - 1)

-- | removeDuplicates function returns a list without duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates =
  foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

valuationSubset :: Valuation -> Valuation -> Bool
valuationSubset v1 v2 = and [ or [ as2 == as1 | as2 <- v2 ] | as1 <- v1 ]

subsumes :: [Valuation] -> [Valuation] -> Bool
subsumes models1 models2 =
  and [ or [ valuationSubset m1 m2 | m2 <- models1 ] | m1 <- models2 ]
