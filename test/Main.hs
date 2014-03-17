module Main where

import Properties
import UnitTests
import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [UnitTests.tests, Properties.tests]
