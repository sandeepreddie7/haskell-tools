{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.Ref1 where

import Language.Haskell.Tools.Refactor as Refactor hiding (LambdaCase)
import GHC (Ghc(..))
import Control.Reference
import Debug.Trace (trace, traceShowId)
import Language.Haskell.Tools.Refactor.Builtin.FlowCtxt (updateContextType)


--Refactoring import in command cli
ref1Refactoring :: RefactoringChoice
ref1Refactoring = ModuleRefactoring "reafactorFindOne" (localRefactoring ref1)

ref1 :: LocalRefactoring
ref1 moduleAST =
        do
            !newAST1 <- liftGhc $ (!~) (biplateRef) (updateContextType) (moduleAST) --renames monad
            !newAST <- liftGhc $ (!~) (biplateRef) (changeFindOne) (newAST1) --changes function
            return newAST

changeFindOne :: Rhs -> Ghc Rhs -- check for pattern with findOne and change the do inside it
changeFindOne rhs@(UnguardedRhs expr@(InfixApp (Var (NormalName (QualifiedName _ (NamePart "findOne")))) _ r@(Do stmts))) =
    let allExprs :: [Expr] = r ^? (biplateRef)
        changedEqs = "KVC.findWithKVConnector dbConf updatedMeshCfg " ++ concat (changeInner <$> allExprs)
    in return $ mkUnguardedRhs $ mkVar $ mkName $ changedEqs
changeFindOne rhs = return rhs

  -- check for pattern with where inside findone, take out the lens and value that equals and construct query with that
changeInner (InfixApp (Var (NormalName (QualifiedName _ (NamePart "where_"))))
                    _
                    (InfixApp (InfixApp _ _ (Var (NormalName (QualifiedName _ (NamePart y))))) _ rhs))
  = let !(rhsNameParts :: [NamePart]) = rhs ^? (biplateRef)
        !rhsNames = fmap (\x -> x ^. simpleNameStr) rhsNameParts
    in "[Is (\\x -> x." ++ y ++ ") (Eq $ " ++ generateChain rhsNames ls ++ ")]"
changeInner _ = mempty

ls = ["val", "toKey", ".", "$"]

generateChain :: [String] -> [String] -> String -- Take all mappinngs applied to the value except few
generateChain []     _  = ""
generateChain (x:xs) ls =
    let t = generateChain xs ls
    in if t == "" then if x `elem` ls then t else x
       else if x `elem` ls then t else x ++ " $ " ++ t


