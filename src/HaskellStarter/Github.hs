module HaskellStarter.Github where

import Control.Monad
import Data.List
import Data.Traversable
-- this Module uses a Github module I found on Hoogle
-- http://hackage.haskell.org/package/github
import Github.Data.Definitions
import Github.Repos.Commits
import HaskellStarter.Util

getMessage :: Commit -> String
getMessage = gitCommitMessage . commitGitCommit

getCommitsFor :: String -> String -> IO [Commit]
getCommitsFor user repo = fmap extract $ commitsFor user repo

getCommitMessagesFor :: String -> String -> IO [String]
getCommitMessagesFor user repo = 
  fmap (fmap getMessage) $ getCommitsFor user repo

printCommitsFor :: String -> String -> IO ()
printCommitsFor user repo = 
  join $ fmap printAll $ getCommitMessagesFor user repo 

