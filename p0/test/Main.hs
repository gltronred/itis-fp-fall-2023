module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List

main :: IO ()
main = defaultMain tests

prop_rev :: Property
prop_rev = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
  reverse (reverse xs) === xs

tests :: TestTree
tests = testGroup "All Tests"
  [ testGroup "Unit Tests"
    [ testCase "3*5 == 15" $ 3*5 @?= 15
    , testCase "2*2 == 4" $ 4 @=? 2*2
    ]
  , testProperty "reverse works" $ prop_rev
  ]
