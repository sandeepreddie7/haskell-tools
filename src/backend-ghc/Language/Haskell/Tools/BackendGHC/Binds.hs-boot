{-# LANGUAGE TypeFamilies, DataKinds #-}
module Language.Haskell.Tools.BackendGHC.Binds where

import ApiAnnotation (AnnKeywordId)
import GHC.Hs.Binds as GHC (HsLocalBinds)
import GHC.Hs.Expr as GHC (Stmt, LHsExpr)
import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..))
import SrcLoc as GHC (Located, SrcSpan)
import GHC.Hs.Extension

trfLocalBinds :: (TransformName n r)=> AnnKeywordId -> HsLocalBinds (GhcPass n) -> Trf (AnnListG AST.ULocalBind (Dom (GhcPass r)) RangeStage)
trfWhereLocalBinds :: (TransformName n r) => SrcSpan -> HsLocalBinds (GhcPass n) -> Trf (AnnMaybeG AST.ULocalBinds (Dom (GhcPass r)) RangeStage)
trfRhsGuard :: (TransformName n r) => Located (Stmt (GhcPass n) (LHsExpr (GhcPass n))) -> Trf (Ann AST.URhsGuard (Dom (GhcPass r)) RangeStage)
trfRhsGuard' :: (TransformName n r) => Stmt (GhcPass n) (LHsExpr (GhcPass n)) -> Trf (AST.URhsGuard (Dom (GhcPass r)) RangeStage)
