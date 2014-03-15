module HaskellStarter.Util (extract ,printAll) where

{-|
   Forcefully pull a value out of an Either.
   This function: 
     Dies with an error if the Either is a Left
     or returns the result if the Either is a Right.

  >>> extract $ Right 10
  10
 -}
extract :: Show a => Either a c -> c
extract = either (error . show) id

{-|
   Print a list of things that can be Shown.
 -}
printAll :: Show a => [a] -> IO ()
printAll xs = mapM_ print xs

