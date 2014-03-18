module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main = glob "src/**/*.hs" >>= doctest
