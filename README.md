![Travis](documentation/Haskelltools.png)

[![Travis](https://img.shields.io/travis/haskell-tools/haskell-tools/master.svg)](https://travis-ci.org/haskell-tools/haskell-tools) [![Hackage](https://img.shields.io/hackage/v/haskell-tools-refactor.svg)](http://hackage.haskell.org/package/haskell-tools-refactor) [![stackage LTS](http://stackage.org/package/haskell-tools-refactor/badge/lts)](http://stackage.org/lts/package/haskell-tools-refactor) [![stackage nightly](http://stackage.org/package/haskell-tools-refactor/badge/nightly)](http://stackage.org/nightly/package/haskell-tools-refactor)

The goal of this project is to create developer tools for the functional programming language Haskell. Currently this project is about refactoring Haskell programs. We have a couple of refactorings working, with support for using them in your editor, or programmatically from command line.

[Available in Atom](https://github.com/nboldi/haskell-tools-atom).

[Demo](http://haskelltools.org) We have a live online demo that you can try

# [Installation instructions](documentation/installation.md)
  - The package is available from hackage and stackage
  - `stack install haskell-tools-daemon haskell-tools-cli --resolver=nightly-[current-date]`
  - When we are not yet on the latest GHC, the only way to install the latest version is to clone this repository and `stack install` it. See the stackage nightly badge above.

# User manuals
   - Use in editor: [Atom](https://github.com/nboldi/haskell-tools-atom/blob/master/documentation/user-manual.md), Sublime (Coming soon...)
   - [Official implemented refactorings](documentation/refactorings.md): The detailed description of the officialy refactorings supported by Haskell-tools Refactor.
   - [ht-refact](documentation/ht-refact.md): A command-line refactorer tool for standalone use.
   - [haskell-tools-demo](documentation/haskell-tools-demo.md): An interactive web-based demo tool for Haskell Tools.

# Contribute

## How to contribute to the Haskell-tools project?

If you encounter a problem, [reporting bugs](documentation/report-bugs.md) always helps us.

If you want to help us by making new tools, refactorings or improving existings ones, here are some useful resources for you.
 - We have a [general overview](documentation/development/framework-overview.md) of the framework, to let you understand the architecture.
 - The [refactoring packages](documentation/development/packages.md) describes how the functionality of the framework is distributed between several packages.
 - A collection of [programming tips](documentation/development/general-tips.md) may help you use the framework as it was intended.
 - The [project information](documentation/development/project-info.md) page tells how to run, test your code, what are the coding and versioning conventions.
 - You can access the API documentation of the [last build](https://haskell-tools.github.io/master/api/index.html), and the [latest release](https://www.stackage.org/nightly/hoogle?q=haskell-tools).

## Write your own refactorings

- Check out the [Tutorials](documentation/development/tutorials.md) for the know-how of refactorings. Please check the [reference](https://github.com/nboldi/references/wiki/References-Tutorial) tutorial also.
- [Guide for writing refactorings](documentation/development/refactoring-guide.md).
- [Limitations](documentation/development/limitations.md)

## Write other tools working with Haskell-tools

(Comming soon...)

## Integrate the tool with your favourite editor.

By implementing a client to handle a simple protocol you can make your favourite editor work with Haskell-tools. Check out the [editor integration](documentation/development/editor-integration.md) tutorial.

## Help to improve the framework

[This section](documentation/development/framework-improvement.md) is for those of you who want to improve the framework to help your refactorings and tools. The [limitations](documentation/development/limitations.md) section could be a good start where to improve the system.

## Using only parse using haskell-tools-parser

For refactorings that doesnt need type check, that is when you do not need any type information for your refactorings, then this section can be used

Working on haskell-tools-parser

1. For getting the executable, Run stack install haskell-tools-parser

2. Use the executables, like <path-to-executable>/ht-parse command --args, command and args depend upon the use case as defined in ProgramOptions.hs file
  For example. To get the function dependency of a module, run ht-parse getFunDeps -p "<modPath>" "<modName>"

3. Writing new refactorings:
      To write new refactors, Check reference of built-in-refactorings
      - [Guide for writing refactorings](documentation/development/refactoring-guide.md).
      Add in program options, add the refactoring in ASTRefactors type

      Check https://github.com/juspay/haskell-tools/pull/4 for reference to write refactorings