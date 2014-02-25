{-# LANGUAGE TemplateHaskell #-}
module Main where

import Properties
import UnitTests

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Runners.Console
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck

main = defaultMain $ [UnitTests.tests, Properties.tests]
