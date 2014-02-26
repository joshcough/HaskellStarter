# HaskellStarter

This project demonstrates how to set up your own real Haskell project, and helps you get a feel for the Haskell ecosystem. Currently, teaching Haskell is *not* a goal of this project, but it may in the future. As for now, basic knowledge of Haskell is assumed, and this project is aimed at people who want to build a real program or library in Haskell.

* Prerequisites
* ghci - Haskell interpreter
* Cabal - Setting up and building a Haskell project
* Hoogle - Finding functions, libraries and documentation
* Building your project on git each commit with Travis
* TODO: Hackage?

## Prerequisites

* Install Haskell (http://www.haskell.org/platform/)
* Basic knowledge of Haskell

## ghci (Haskell Interpreter)

Run ghci at the command line by simply typing ghci:
      
    > ghci

Input expressions
 
    prelude> 5 + 5
    10
    prelude> let x = 10
    prelude> x + x
    20

Get types with :t

    prelude> :t x
    x :: Num a => a
    prelude> let x = 7 :: Int
    prelude> :t x + x
    x + x :: Int

Load files with :load

    prelude> :load Goop
    *Goop> :load Hoop
            TODO: fix this, load goop, run an expression from it. 
            add something other than x and y to it.

Quit ghci

    prelude> :q
    prelude> ^d

## Cabal (Haskell project configuration

While ghci is useful for playing with Haskell code, it doesn't enable you to build libraries and programs. Cabal is the canonacal tool for building Haskell code. This section explains using cabal, but where it lacks, you can get more info at: http://www.haskell.org/cabal/.
  
  * Anatomy of a Cabal file
    * Libraries
    * Dependencies
    * Tests
    * Executables

Building, Installing and running tests with Cabal
  
Installing your libraries and executables (TODO: where exactly do they go? show it)

    > cabal install

Running your tests
        
    > cabal install --enable-tests 
          (TODO: is there a way to run tests without installing? especially all the executables)
Running executables

    * Remember to add ~/.cabal/bin to your PATH
    * > HaskellStarter "joshcough" "HaskellStarter"

## Hoogle (http://www.haskell.org/hoogle/)

Finding functions, libraries, documentation

## Travis

Building your project automatically after a git push

