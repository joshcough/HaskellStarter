module Main where

import Properties
import UnitTests
import Test.Tasty(defaultMain, testGroup)

main = defaultMain $ testGroup "All tests" [UnitTests.tests, Properties.tests]
