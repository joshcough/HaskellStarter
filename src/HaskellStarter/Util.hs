module HaskellStarter.Util 
(
  extract
 ,printAll
) where

--import Control.Monad
--import Data.List
--import Data.Traversable

{-
 - We're not dealing with error messages yet in this tutorial
 - So for now, we will just die if there was an error.
 - This function 
 -   dies on an error
 -   or returns the real result on success
 -}
extract :: Show a => Either a c -> c
extract = either (error . show) id

{-
 - Print a list of things that can be Shown.
 -}
printAll :: Show a => [a] -> IO ()
printAll xs = mapM_ print xs
