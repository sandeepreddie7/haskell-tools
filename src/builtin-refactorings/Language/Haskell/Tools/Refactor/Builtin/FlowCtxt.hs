{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.FlowCtxt where

import Language.Haskell.Tools.Refactor as Refactor hiding (LambdaCase)
import GHC (Ghc(..))
import InstEnv as GHC
import Unify as GHC
import Type as GHC
import Name as GHC
import Var as GHC
import UniqSupply as GHC
import Unique as GHC
import TysWiredIn as GHC
import TysPrim as GHC
import PrelNames as GHC
import ConLike as GHC
import PatSyn as GHC
import BasicTypes as GHC

import Control.Reference
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List
import qualified Data.Map.Strict as SMap (empty, toList)

import Debug.Trace (trace, traceShowId)

import Outputable
import Language.Haskell.Tools.PrettyPrint

import Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.BackendGHC.GHCUtils (occName, fromSrcText)
import Language.Haskell.Tools.BackendGHC.Names
import Language.Haskell.Tools.BackendGHC.Patterns (trfPattern)
import Language.Haskell.Tools.BackendGHC.Types (trfType)

import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

import Control.Monad.IO.Class

import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances

-- import Language.Haskell.Tools.ASTDebug

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

changeContextTypeRefactoring :: RefactoringChoice
changeContextTypeRefactoring = ModuleRefactoring "ctxt" (localRefactoring changeContextType)

changeContextType :: LocalRefactoring
changeContextType moduleAST =
        do
            _ <- liftIO $ putStrLn $ srcInfoDebug $ (moduleAST)
            !newAST <- liftGhc $ (!~) (biplateRef) (updateContextType) (moduleAST)
            return newAST

updateContextType :: Assertion -> Ghc Assertion
updateContextType assertion@(ClassAssert (NormalName (QualifiedName _ (NamePart "Transactionable"))) (AnnList newTyps))
    = return $ (mkClassAssert (mkName "MonadFlow") newTyps)
updateContextType assertion = return assertion

-- changeFindOne :: Rhs -> Ghc Rhs
-- changeFindOne rhs@(UnguardedRhs expr@(InfixApp (Var (NormalName (QualifiedName _ (NamePart "findOne")))) _ r@(Do stmts))) = do
--     let allExprs :: [Expr] = r ^? (biplateRef)
--         innerExpr = filter (\x -> x /= mempty) (changeInner <$> allExprs)
--         concateNatedExprs = if length innerExpr > 1 then take (length innerExpr `subtract` 1) ("And " ++ concatMap (\x -> x ++ ",") innerExpr) else concat innerExpr
--         changedEqs = "KVC.findWithKVConnector dbConf updatedMeshCfg " ++ concateNatedExprs
--     return $ mkUnguardedRhs $ mkVar $ mkName $ changedEqs
-- changeFindOne rhs = return rhs

