import           Test.QuickCheck

import           DefeasibleLogic
import           DefeasibleTypes
import           PropositionalLogic
import           PropositionalTypes

main :: IO ()
main = do
  quickCheck parserTest
  quickCheck penguinRCTest
  quickCheck leakyboatRCTest

parserTest = do
  let form = str2form "A And B"
  form == And (Atom "A") (Atom "B")

penguinRCTest = do
  let dkb =
        [ typically (str2form "b Implies f")
        , typically (str2form "p Implies Not f")
        , typically (str2form "b Implies w")
        ]
  let kb = [str2form "p Implies b", str2form "R Implies b"]
  let jkb   = (kb, dkb)
  let query = typically (str2form "p Implies b")
  entailsRC jkb query

leakyboatRCTest = do
  let dkb =
        [ typically (str2form "boat Implies floats")
        , typically (str2form "leaky Implies boat")
        , typically (str2form "leaky Implies Not floats")
        , typically (str2form "FlyingDutchman Implies boat")
        , typically (str2form "FlyingDutchman Implies leaky")
        ]
  let kb    = []
  let jkb   = (kb, dkb)
  let query = typically (str2form "FlyingDutchman Implies Not floats")
  entailsRC jkb query
