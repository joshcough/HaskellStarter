module HaskellStarter.Github where

import Control.Monad
import Data.List
import Data.Traversable
import Github.Data.Definitions
import Github.Repos.Commits

extract :: Show a => Either a c -> c
extract = either (error . show) id

printAll :: Show a => [a] -> IO ()
printAll xs = mapM_ print xs

getMessage :: Commit -> String
getMessage = gitCommitMessage . commitGitCommit

messages :: Functor f => f Commit -> f String
messages = fmap (gitCommitMessage . commitGitCommit)

getCommitsFor :: String -> String -> IO [Commit]
getCommitsFor user repo = fmap extract $ commitsFor user repo

getCommitMessagesFor :: String -> String -> IO [String]
getCommitMessagesFor user repo = fmap messages $ getCommitsFor user repo

printCommitsFor :: String -> String -> IO ()
printCommitsFor user repo = join $ fmap printAll $ getCommitMessagesFor user repo 

