{-# LANGUAGE TemplateHaskell #-}
module Properties where

import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck

prop_list_reverse_reverse :: [Int] -> Bool
prop_list_reverse_reverse list = list == reverse (reverse list)

prop_list_length :: [Int] -> Int -> Bool
prop_list_length list i = length (i : list) == 1 + length list

tests = $testGroupGenerator
