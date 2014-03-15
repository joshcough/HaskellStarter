module HaskellStarter.Github where

-- this Module uses a Github module I found on Hoogle
-- http://hackage.haskell.org/package/github
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
  commits        <- fmap extract $ commitsFor user repo
  commitMessages <- return $ fmap getMessage commits
  printAll commitMessages
