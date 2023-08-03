{-# LANGUAGE RankNTypes #-}

module Language.Haskell.Tools.Parser.Interface where

import Language.Haskell.Tools.Parser.ProgramOptions
import Language.Haskell.Tools.Parser.SplitModule
import Options.Applicative
import Data.Functor (($>))

main :: IO ()
main = do
    parserComb <- execParser (info opts idm)
    case parserComb of
        Refact astParseConfig -> 
            case functionality astParseConfig of
                FunctionDependency -> print <$> getFunctionDeps (modulePath astParseConfig) (moduleName astParseConfig) $> ()
                _ -> pure ()
        SplitAndWrite writeFileConfig -> print <$> (splitAndWrite (modPath writeFileConfig) (modName writeFileConfig) (clusters writeFileConfig) (funDeps writeFileConfig)) $> ()
