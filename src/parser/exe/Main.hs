{-# LANGUAGE RankNTypes #-}

module Main where

import Language.Haskell.Tools.Parser.ProgramOptions
import Language.Haskell.Tools.Parser.SplitModule
import Language.Haskell.Tools.Parser.LetRefactoring
import Options.Applicative
import Data.Functor (($>), void)

main :: IO ()
main = do
    parserComb <- execParser (info opts idm)
    case parserComb of
        Refact astParseConfig -> 
            case functionality astParseConfig of
                FunctionDependency -> do
                    deps <- getFunctionDeps (modulePath astParseConfig) (moduleName astParseConfig)
                    print deps
                    pure ()
                LetRefactoring -> removeMultiLets (modulePath astParseConfig) (moduleName astParseConfig) $>  ()
        SplitAndWrite writeFileConfig -> void (splitAndWrite (modPath writeFileConfig) (modName writeFileConfig) (clusters writeFileConfig) (funDeps writeFileConfig))
