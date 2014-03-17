module Main where

import HaskellStarter.CommitPrinter
import System.Environment

main = do
  args <- getArgs
  printCommitsFor (args !! 0) (args !! 1)

