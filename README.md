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

ghci is a simple way to get started playing with Haskell.

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

While ghci is useful for playing with Haskell code, it doesn't enable you to build libraries and programs. Cabal is the canonacal tool for building Haskell code. This section explains using cabal, but where it lacks, you can get more info at: http://www.haskell.org/cabal/. This section will cover:
  
  * Anatomy of a Cabal file
    * Libraries
    * Dependencies
    * Tests
    * Executables
  * Building libraries and executables
  * Running tests

### Specifying a Library

    library 
      hs-source-dirs: src

      exposed-modules:
        HaskellStarter.Github
        HaskellStarter.Util

      build-depends:
        base   >= 4 && < 5,
        github >= 0.7.4

### Specifying Executables

This example specifies an exectuable called githubCommitPrinter. githubCommitPrinter is a command line program that takes two arguments, a username and a project name, and prints the last 30 commits for that project.

    executable githubCommitPrinter
      hs-source-dirs: main
      main-is: Main.hs
      build-depends: base < 5, haskell-starter

I think this is mostly self explanatory, but I'll do so anyway.

  * Line 1 starts the executable block, and names it. You may have many different executables in one cabal file.
  * Line 2 specifies what directory the code for the executable lives in. 
  * Line 3 specifies the Haskell file that contains the main function. (Open question: can the module be named anything?)
  * Line 4 specifies all of the packages that the executable depends on. Notice here that githubCommitPrinter depends on the haskell-starter library. Cabal doesn't implicitely add your libraries to executables.

### Building libraries, installing executables and running tests with Cabal
  
Installing your libraries and executables 

    > cabal install

Running your tests
        
    > cabal install --enable-tests 

  (Open question: is there a way to run tests without installing all of the executables?)

Running executables

    By default, Cabal installs executables to ~/.cabal/bin. By adding that to your PATH, you can run your executables immediately.

    > HaskellStarter "joshcough" "HaskellStarter"

## Hoogle (http://www.haskell.org/hoogle/)

Hoogle is the go to place for finding functions, libraries, documentation.

## Travis

Building your project automatically after a git push.

