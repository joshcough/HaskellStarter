{-|
  This module allows you to print the commits messages
  in a github repo.
 -}
module HaskellStarter.CommitPrinter (printCommitsFor) where

import Control.Applicative
import Github.Repos.Commits
import HaskellStarter.Util

{-|
   Get the actual commit message from a Commit.
 -}
getMessage :: Commit -> String
getMessage = gitCommitMessage . commitGitCommit

{-|
  Print all of the commits messages for a given user and repo.
 -}
printCommitsFor :: String -> String -> IO ()
printCommitsFor user repo = do
  commits <- extract <$> commitsFor user repo
  printAll $ getMessage <$> commits
