{-# LANGUAGE TemplateHaskell #-}

module Properties where

import Test.Framework.Providers.QuickCheck2
import Test.Framework.Runners.Console
import Test.Framework.TH
import Test.QuickCheck

prop_list_reverse_reverse :: [Int] -> Bool
prop_list_reverse_reverse list = list == reverse (reverse list)
tests = $testGroupGenerator
