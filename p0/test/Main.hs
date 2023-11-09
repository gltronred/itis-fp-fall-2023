module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import MyLib

main :: IO ()
main = defaultMain tests

prop_rev :: Property
prop_rev = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
  rev (rev xs) === xs

prop_opaque :: Property
prop_opaque = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
  let op = Opaque xs
  op /== op

prop_div10000 :: Property
prop_div10000 = property $ do
  x <- forAll $ Gen.int (Range.linear 1 100000)
  x `mod` 10000 /== 0

tests :: TestTree
tests = testGroup "All Tests"
  [ testGroup "Unit Tests"
    [ testCase "3*5 == 15" $ 3*5 @?= 15
    , testCase "2*2 == 4" $ 4 @=? 2*2
    , testCase "rev []" $ rev [] @?= ([] :: [Int])
    , testCase "rev [1,2,3]" $
      rev [1,2,3] @?= [3,2,1]
    ]
  , testProperty "reverse works" $ prop_rev
  , testProperty "strange opaque value" prop_opaque
  , testProperty "all numbers do not divide 10000" $
    prop_div10000
  ]
