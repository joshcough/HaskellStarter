# HaskellStarter

[![Build Status](https://travis-ci.org/joshcough/HaskellStarter.png?branch=master)](https://travis-ci.org/joshcough/HaskellStarter)

This project demonstrates how to set up your own real Haskell project, and helps you get a feel for the Haskell ecosystem. Currently, teaching Haskell is *not* a goal of this project, but it may in the future. As for now, basic knowledge of Haskell is assumed, and this project is aimed at people who want to build a real program or library in Haskell.

* [Getting started with this project](#getting-started-with-this-project)
* [Prerequisites](#prerequisites)
  * [ghci - Haskell interpreter](#ghci)
  * [Hoogle - Finding functions, libraries and documentation](#hoogle)
* [Cabal - The Haskell build tool](#cabal)
  * [Libraries](#creating-a-library)
    * [Modules](#creating-a-library)
    * [Configuration](#configuring-the-library-in-cabal)
    * [Building and Installing your Library](#building-and-installing-your-library)
  * [Haddock - Haskell documentation](#haddock)
  * [Dependencies](#understanding-dependencies)
  * [Executables](#executables)
    * [Configuring an executable](#configuring-an-executable-in-cabal)
    * [Installing and Running Executables](#installing-and-running-executables)
  * [Tests](#tests)
    * [Unit tests with HUnit](#hunit)
    * [Properties with QuickCheck](#quickcheck)
    * [test-framework](#test-framework)
    * [Configuring a test suite](#configuring-a-test-suite-in-cabal)
    * [Running Tests](#running-tests)
    * [doctests](#doctest)
  * Hackage - Publishing your library
* [Travis - Building your project on git each commit](#travis)
* [Further Reading](#further-reading)

## Getting started with this project

The best way to get started with this project is to simply clone it, and poke around.

`git clone https://github.com/joshcough/HaskellStarter.git`

Some valuable commands to play with (all of which will be explained) are:

```
> cabal update
> cabal install github
> cabal build
> cabal test
> cabal install
```

## Prerequisites

As stated, this project assumes some basic knowledge of Haskell. If you don't have that, I recommend [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/). Also, if you haven't already, go install the [Haskell Platform](http://www.haskell.org/platform/). 

### ghci

In order to have some basic knowledge of Haskell, it's highly likely that you've played with ghci, but a refresher still helps. 

ghci is a simple way to get started playing with Haskell, and is essential for testing out functions and types during development.

Run ghci at the command line by simply typing ghci:
      
    > ghci

Input expressions

``` 
prelude> 5 + 5
10
prelude> let x = 10
prelude> x + x
20
```
Get types with :t

```
prelude> :t x
x :: Num a => a
prelude> let x = 7 :: Int
prelude> :t x + x
x + x :: Int
```

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

    > cabal install github

## Cabal

While ghci is useful for playing with Haskell code, it doesn't enable you to build libraries and programs. Cabal (Common Architecture for Building Applications and Libraries) is the canonacal tool for building Haskell code. This section explains using cabal, but where it lacks, you can get more info at: http://www.haskell.org/cabal/.

### Getting Started

A few helpful commands for getting started with Cabal:

* `cabal --help` shows you all the Cabal commands. Highly recommended.
* `cabal init` runs you through a series of questions to start a new project.
* `cabal update` updates Cabal so tht it has all of the latest package information.
* `cabal install` installs a package. It takes a single argument, like `cabal install github` which was used above.
  
### Creating a Library

#### Modules

In order to have a library, we need some code. :) I usually put my code in a directory called `src`, but any directory name you want is fine. In this project we have two modules, `HaskellStarter.Util`, which just contains some simple utility functions, and `HaskellStarter.CommitPrinter` which uses the github library previously mentioned to print commits for a repo. 

Let's take a look at `HaskellStarter.Util`:

```Haskell
module HaskellStarter.Util (extract, printAll) where

extract :: Show a => Either a c -> c
extract = either (error . show) id

printAll :: [String] -> IO ()
printAll xs = mapM_ print xs
```

Don't worry too much yet about what this code actually does; we'll document it shortly. 

And `HaskellStarter.CommitPrinter`:

```Haskell
module HaskellStarter.CommitPrinter where

import Control.Applicative
import Github.Repos.Commits
import HaskellStarter.Util

getMessage :: Commit -> String
getMessage = gitCommitMessage . commitGitCommit

printCommitsFor :: String -> String -> IO ()
printCommitsFor user repo = do
  commits <- extract <$> commitsFor user repo
  printAll $ getMessage <$> commits
```

We will document this code shortly too, but do notice that it imports `Github.Repos.Commits`, which is a module in the github library.

#### Configuring the library in Cabal

A cabal file can only have one library (but you're not required to have one). Here's the configuration for it:

    library 
      hs-source-dirs: src
      exposed-modules:
        HaskellStarter.CommitPrinter
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

Let's add some documentation to the code, and then generate pretty html from it.

First `HaskellStarter.Util`:

```Haskell
{-|
 This module contains some simple utility functions.
 It exports two functions, extract and printAll.
 -}
module HaskellStarter.Util (extract, printAll) where

{-|
   Forcefully pull a value out of an Either.
   This function: 
     * Returns the result if the Either is a Right.
     * Dies with an error if the Either is a Left.
 -}
extract :: Show a => Either a c -> c
extract = either (error . show) id

{-|
   Print a list of Strings, one per line.
 -}
printAll :: [String] -> IO ()
printAll xs = mapM_ print xs
```

And `HaskellStarter.CommitPrinter`:

```Haskell
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
```

Hopefully this documentation helps explain what the code does. If not, feel free to fix it and send me a pull request.

With the docs in our source code, generating pretty html from them is simple:

    > cabal haddock

Which outputs this info:

    Haddock coverage:
     100% (  3 /  3) in 'HaskellStarter.Util'
     100% (  2 /  2) in 'HaskellStarter.CommitPrinter'
    Documentation created: dist/doc/html/haskell-starter/index.html

Now, open up `dist/doc/html/haskell-starter/index.html` and see the glory. Notice that only publicly exposed modules are added to the documentation. 

### Understanding Dependencies

TODO

### Executables

A library is a collection of code that you can depend on, but cannot actually execute. Fortunately, you can build executables with Cabal very easily. To do this, we first need a module with a main function. Here is `main/Main.hs` from this project:

```Haskell
module Main where

import HaskellStarter.CommitPrinter
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  printCommitsFor (args !! 0) (args !! 1)
```

This is a command line program that takes two arguments - a username and a project name, and prints the commits for that project.

#### Configuring an executable in Cabal

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

### Tests  

In Haskell and Cabal there are a _lot_ of different test libraries and frameworks, and it's difficult to choose which to use. Here, I'll explain briefly:

* HUnit - a library for writing unit tests
* QuickCheck - a library for writing properties 
* test-framework - a framework for organizing and running unit tests and properties
* doctest - inject tests directly into your documentation

#### HUnit

[HUnit](#http://hunit.sourceforge.net/) is "is a unit testing framework for Haskell, similar to the JUnit tool for Java."

It allows you to write very simple assertions of this form:

```Haskell
actual @?= expected
```

And if actual doesn't equal expected, then you will get a test error.

I've written a very simple example in `test/UnitTests.hs`* that I think is very self explanatory:

```Haskell
module UnitTests where

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = testGroup "HUnit tests" [
  testCase "a passing test!"       $ 5 @?= 5
 ,testCase "another passing test!" $ 6 @?= 6 ]
```

*This doesn't currently import anything in the library. The reason for this is a little subtle - `HaskellStarter.Utils` is the only module with easily testable functions, but it is not exposed, so we don't have access to it! There are a couple ways around this, but the easiest is to simply expose all modules that you wish to test.

We will see how to run these tests shortly.

#### QuickCheck

[QuickCheck](#http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck2) is a library for writing properties for your functions, and a framework for testing those properties with random inputs. Here is a simple example property that for all lists of integers, tests that the reverse or the reverse of that list is the equal to that list.

```Haskell
prop_list_reverse_reverse :: [Int] -> Bool
prop_list_reverse_reverse list = list == reverse (reverse list)
```

This property is put into a module in `tests/Properties.hs`:

```Haskell
{-# LANGUAGE TemplateHaskell #-}
module Properties where

import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck

prop_list_reverse_reverse :: [Int] -> Bool
prop_list_reverse_reverse list = list == reverse (reverse list)

prop_list_length :: [Int] -> Int -> Bool
prop_list_length list i = length (i : list) == 1 + length list

tests = $testGroupGenerator
```

This module has a couple of very simple properties in it (once again, unrelated to the code in the library). We will see how to run these shortly, as well.

#### test-framework

Before we can run our tests, we need to package them up into a module with a main function. We do that using test-framework. Here are the contents of `test/Main.hs`:

```Haskell
module Main where

import Properties
import UnitTests
import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [UnitTests.tests, Properties.tests]
```

This module provides a main function using the defaultMain function from test-framework. It takes a list of test groups as an argument, and runs all the tests in those groups. It also takes care of printing fancy messages for successes and failures.

#### Configuring a test suite in Cabal

Before we can run our tests, we need to configure our test suite in Cabal:

```
-- configuration for Unit tests and properties
test-suite unit-tests-and-properties
  type:           exitcode-stdio-1.0
  main-is:        Main.hs
  hs-source-dirs: test
  build-depends:
    base,
    HUnit,
    QuickCheck                 >= 2.4,
    test-framework             >= 0.6,
    test-framework-hunit,
    test-framework-quickcheck2 >= 0.2,
    test-framework-th          >= 0.2
```

Don't worry too much about the details here. Just know that the tests are in the `test` directory, and `Main.hs` is in there. Hopefully soon I'll be able to provide more info here, and/or make the configuration slightly less verbose.

#### Running Tests

Now that we have our test suite configured, running it is very easy:

    > cabal test

You can also pass the `--enable-tests` flag to `cabal install`, which will run all of your tests, and only install the library if all of the tests pass:
        
    > cabal install --enable-tests 

#### doctest

"[doctest](https://github.com/sol/doctest-haskell) is a small program, that checks examples in Haddock comments. It is similar to the popular Python module with the same name."

Let's add some tests into our documentation for the `extract` function in `HaskellStarter.Util`:

```Haskell
{-|
   Forcefully pull a value out of an Either.
   This function: 
     * Returns the result if the Either is a Right.
     * Dies with an error if the Either is a Left.
  >>> extract $ Right 10
  10
  >>> extract $ Right "hello, world"
  "hello, world"
 -}
extract :: Show a => Either a c -> c
extract = either (error . show) id
```

In case it's hard to notice, I've addded:

```
  >>> extract $ Right 10
  10
  >>> extract $ Right "hello, world"
  "hello, world"
```

The first example says to run the extract function with `Right 10`, and expect to get back the value 10. The second is nearly identical.

In order to run doctests, we need to have a main function, and configure it in Cabal. 

Here are the contents of `test/Doctest.hs`:

```Haskell
module DocTest where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main = glob "src/**/*.hs" >>= doctest
```

and here is the Cabal test suite configuration:

```
test-suite doctest
  type:           exitcode-stdio-1.0
  main-is:        DocTest.hs
  hs-source-dirs: test
  build-depends:  base, doctest == 0.9.*, Glob == 0.7.*
```

All `test-suite` configurations get ran when you execute `cabal test`, so this there is nothing else additional needed.

## Travis

Travis (travis-ci.org) is a service for building your project automatically after a git push. It notifies you via email if the build fails. This project is already set up to use travis. Specifically, `.travis.yml` and the `travis` directory. I don't have time to provide details on the contents of these files right now, but hope to soon.

## Further Reading

Here is a bunch of links (in no particular order) that I found useful in creating this project, and for Haskell development in general.

Hoogle:
* http://www.haskell.org/hoogle/ (This is probably the most important link, for me.)

Hackage:
* http://hackage.haskell.org
* http://hackage.haskell.org/packages
* http://hackage.haskell.org/upload

Libraries:
* http://hackage.haskell.org/package/github

Haddock:
* http://www.haskell.org/haddock/#Overview

doctest:
* https://github.com/sol/doctest-haskell
* http://hackage.haskell.org/package/doctest

Testing:
* http://hunit.sourceforge.net/
* http://hunit.sourceforge.net/HUnit-1.0/Guide.html
* http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck2
* http://hackage.haskell.org/package/test-framework
* https://github.com/sol/doctest-haskell

Books:
* http://learnyouahaskell.com/
* http://book.realworldhaskell.org/

Cabal:
* http://www.haskell.org/cabal/

Travis:
* https://travis-ci.org/

Other getting started links:
* http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/
