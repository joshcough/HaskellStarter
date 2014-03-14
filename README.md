# HaskellStarter

This project demonstrates how to set up your own real Haskell project, and helps you get a feel for the Haskell ecosystem. Currently, teaching Haskell is *not* a goal of this project, but it may in the future. As for now, basic knowledge of Haskell is assumed, and this project is aimed at people who want to build a real program or library in Haskell.

* [Prerequisites](#prerequisites)
  * [ghci - Haskell interpreter](#ghci)
  * [Hoogle - Finding functions, libraries and documentation](#hoogle)
* [Cabal - The Haskell build tool](#cabal)
  * [Libraries](#creating-a-library)
    * [Configuration](#creating-a-library)
    * [Building and Installing your Library](#building-and-installing-your-library)
  * [Haddock - Haskell documentation](#haddock)
  * [Dependencies](#understanding-dependencies)
  * [Tests](#tests)
    * Unit tests and properties
    * doctests (Documentation tests)
    * [Running Tests](#running-tests)
  * [Executables](#specifying-executables)
    * [Installing and Running Executables](#installing-and-running-executables)
  * Hackage - Publishing your library
* [Travis - Building your project on git each commit](#travis)

## Prerequisites

As stated, this project assumes some basic knowledge of Haskell. If you don't have that, I recommend [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/). Also, if you haven't already, go install the [Haskell Platform](http://www.haskell.org/platform/). 

### ghci

In order to have some basic knowledge of Haskell, it's highly likely that you've played with ghci, but a refresher still helps. 

ghci is a simple way to get started playing with Haskell, and is essential for testing out functions and types during development.

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

### Hoogle

Hoogle (http://www.haskell.org/hoogle/) is the go to place for finding: 

* Functions
* Libraries
* Documentation
* Links to source code
* etc...

Maybe we want to write a library that does some fun stuff with the Github API (http://developer.github.com/v3/). Let's go search Hoogle to see if there is anything already there to help us:

   http://www.haskell.org/hoogle/?hoogle=github

Yay, we've discovered a Github package - http://hackage.haskell.org/package/github, complete with everything listed above. Poke around and find out more. After you're done poking, you should install the package:

    cabal install github

## Cabal

While ghci is useful for playing with Haskell code, it doesn't enable you to build libraries and programs. Cabal (Common Architecture for Building Applications and Libraries) is the canonacal tool for building Haskell code. This section explains using cabal, but where it lacks, you can get more info at: http://www.haskell.org/cabal/.

### Getting Started

A few helpful commands for getting started with Cabal:

* `cabal --help` shows you all the Cabal commands. Highly recommended.
* `cabal init` runs you through a series of questions to start a new project.
* `cabal install` installs a package. It takes a single argument, like `cabal install github` which was used above.
  
### Creating a Library

A cabal file can only have one library (but you're not required to have one).

    library 
      hs-source-dirs: src

      exposed-modules:
        HaskellStarter.Github

      other-modules:
        HaskellStarter.Util

      build-depends:
        base >= 4 && < 5, github >= 0.7.4

* `hs-source-dirs` is a list of directories to find your source files in, relative to the root directory of your project. Here, we only use one, but you can also say: `hs-source-dirs: src1 src2 goober/joober`
* `exposed-modules` specifies the modules in the libraries public API.
* `other-modules` specifies modules that aren't publicly exposed, but are still part of the library.
* `build-depends` will be explained in the section [Understanding Dependencies](#understanding-dependencies), but for now, it is enough to know that this library depends on two other libraries: base, and github.

A quick note about modules: Cabal must know about all of your modules in the library, so they must be specified in `exposed-modules` or `other-modules`. I find this good and bad - on one hand it allows you to have Haskell files in your source directory that you don't want to be compiled, on the other, it forces you to list all of your modules.

#### Building and Installing your Library

Building your library is easy:

    > cabal build

If you want to build other projects that depend on your library, you can install it locally:

    > cabal install

### Haddock

Let's add some documentation to the code:

TODO

With the docs in our source code, generating pretty html from them is simple:

    cabal haddock

### Understanding Dependencies

TODO

### Tests  

In Haskell and Cabal there are a _lot_ of different test libraries and frameworks, and it's difficult to choose which to use. Here, I'll explain briefly:

* HUnit - a library for writing unit tests
* QuickCheck - a library for writing properties 
* test-framework - A framework for organizing and running unit tests and properties

#### Running Tests

Running your tests is also easy:

    > cabal test

You can also pass the `--enable-tests` flag to `cabal install`, which will run all of your tests, and only install the library if all of the tests pass:
        
    > cabal install --enable-tests 

### Specifying Executables

A library is a collection of code that you can depend on, but cannot actually execute. Fortunately, you can build executables with Cabal very easily. To do this, we first need a module with a main function. Here is `main/Main.hs` from this project:

```Haskell
module Main where

import HaskellStarter.Github
import System.Environment

main = do
  args <- getArgs
  printCommitsFor (args !! 0) (args !! 1)
```

This is a command line program that takes two arguments - a username and a project name, and prints the commits for that project.

Configuring an exectuable in Cabal is very simple:

    executable githubCommitPrinter
      hs-source-dirs: main
      main-is: Main.hs
      build-depends: base < 5, haskell-starter

* `executable githubCommitPrinter` starts the executable block, and names it. You may have many different executables in one cabal file.
* `hs-source-dirs` is a list of directories to find source files.
* `main-is` specifies the Haskell file that contains the `main` function. `main` must have type `IO ()`.
* `build-depends` is the same as it is in the library definition. Notice here that githubCommitPrinter depends on the haskell-starter library. Cabal doesn't implicitely add your library to executables.

#### Installing and Running Executables

`cabal install` installs all executables in your project, as well as the library (if there is one). By default, Cabal installs executables to ~/.cabal/bin. By adding that to your PATH, you can run your executables immediately.

    > githubCommitPrinter joshcough HaskellStarter

## Travis

Travis (travis-ci.org) is a service for building your project automatically after a git push. It notifies you via email if the build fails. This project is already set up to use travis. Specifically, `.travis.yml` and the `travis` directory. I don't have time to provide details on the contents of these files right now, but hope to soon.

