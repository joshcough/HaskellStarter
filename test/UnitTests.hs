module UnitTests where

import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "HUnit tests" [
  testCase "a passing test!"       $ 5 @?= 5
 ,testCase "another passing test!" $ 6 @?= 6 ]
