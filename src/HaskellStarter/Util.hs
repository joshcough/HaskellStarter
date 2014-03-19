{-|
 This module contains some simple utility functions.
 It exports two functions, extract and printAll.
 -}
module HaskellStarter.Util (extract, printAll) where

{-|
   Forcefully pull a value out of an 'Either'.

   This function: 

   * Returns the result if the 'Either' is a 'Right'.

   * Dies with an error if the 'Either' is a 'Left'.

   >>> extract $ Right 10
   10

   >>> extract $ Right "hello, world"
   "hello, world"
 -}
extract :: Show a => Either a c -> c
extract = either (error . show) id

{-|
   Print a list of Strings, one per line.
 -}
printAll :: [String] -> IO ()
printAll xs = mapM_ print xs

