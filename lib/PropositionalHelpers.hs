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

-- | assigns function searches for an atom's assigned value in a given valuation
assigns :: Valuation -> Name -> Bool
assigns [] _ = error "Atom not found in given valuation"
assigns ((a, b) : ve) n | n == a    = b
                        | otherwise = assigns ve n

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

{-
valuationSubset :: Valuation -> Valuation -> Bool
valuationSubset inner outer =
  and [ or [ as2 == as1 | as2 <- outer ] | as1 <- inner ]
-}

subValuation :: [Valuation] -> Valuation -> Bool
subValuation (v : []) superval = and
  [ or [ (supN == subN) && (supB == subB) | (supN, supB) <- superval ]
  | (subN, subB) <- v
  ]
subValuation subvals superval = or [ subValuation [v] superval | v <- subvals ]


subsumes4 :: [Valuation] -> [Valuation] -> Bool
subsumes4 subsumer subsumed = and [ subValuation subsumed v | v <- subsumer ]

subList :: Valuation -> Valuation -> Bool
subList [] [] = True
subList _  [] = False
subList [] _  = True
subList ((xn, xb) : xs) ((yn, yb) : ys) | xn == yn && xb == yb = subList xs ys
                                        | otherwise = subList ((xn, xb) : xs) ys

subsumes :: [Valuation] -> [Valuation] -> Bool
subsumes subsumer subsumed =
  and [ and [ subList ed er | er <- subsumer ] | ed <- subsumed ]

subsumes2 :: [Valuation] -> [Valuation] -> Bool
subsumes2 subsumer subsumed = and
  [ or [ n == n2 && b == b2 | er <- subsumer, (n2, b2) <- er ]
  | v      <- subsumed
  , (n, b) <- v
  ]

subsumes3 :: [Valuation] -> [Valuation] -> Bool
subsumes3 subsumer subsumed = and
  [ and
      [ or
          [ nSubsumed == nSubsumer && bSubsumed == bSubsumer
          | (nSubsumed, bSubsumed) <- vSubsumed
          , (nSubsumer, bSubsumer) <- vSubsumer
          ]
      | vSubsumed <- subsumed
      ]
  | vSubsumer <- subsumer
  ]
