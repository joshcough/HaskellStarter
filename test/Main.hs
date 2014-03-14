{-# LANGUAGE TemplateHaskell #-}
module Main where

import Properties
import UnitTests
import Test.Framework.Runners.Console

main = defaultMain $ [UnitTests.tests, Properties.tests]
