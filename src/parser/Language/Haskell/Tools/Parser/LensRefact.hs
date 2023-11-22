{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Parser.LensRefact where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint
import Data.List
import GHC
import Control.Reference
import Language.Haskell.Tools.Parser.ParseModule
import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances
import Control.Monad.Extra
import qualified Data.HashMap.Strict as HM
import Data.Char (toLower)
import Data.List.Extra (replace)

lensRefactoring :: String -> String -> IO ()
lensRefactoring modulePath moduleName = do
    moduleAST <- moduleParser modulePath moduleName
    newAST <- (!~) (biplateRef) (lensRefact) (moduleAST)
    writeFile (modulePath <> (replace "." "/" moduleName) <> ".hs") (prettyPrint newAST)

lensRefact :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> IO (Ann UExpr (Dom GhcPs) SrcTemplateStage)
lensRefact expr@(Ann _ (UInfixApp
                          lhs@(Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart _))))))))
                          (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".^.")))))) 
                          rhs@(Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart fieldName)))))))))) = 
    if fieldName /= "_id" then
        let infixApp = mkInfixAppForRanged lhs (mkUnqualOpForRanged "^.")  rhs
            app = mkAppForRanged (mkVar' $ mkName' "fromMaybe") (mkInfixAppForRanged ((mkVar' $ mkName' "mempty")) (mkUnqualOpForRanged "$") (infixApp))
        in pure $ app
    else pure expr
traverseOverUValBind expr = pure expr
