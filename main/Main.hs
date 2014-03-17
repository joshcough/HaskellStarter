module Main where

import HaskellStarter.CommitPrinter
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  printCommitsFor (args !! 0) (args !! 1)

