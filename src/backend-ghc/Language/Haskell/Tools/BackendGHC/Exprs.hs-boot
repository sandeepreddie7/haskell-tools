{-# LANGUAGE TypeFamilies, DataKinds #-}
module Language.Haskell.Tools.BackendGHC.Exprs where

import GHC.Hs.Expr as GHC (HsExpr, HsCmd)
import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..))
import SrcLoc as GHC (Located)
import GHC.Hs.Extension

trfExpr :: (TransformName n r)  => Located (HsExpr (GhcPass n)) -> Trf (Ann AST.UExpr (Dom (GhcPass r)) RangeStage)
trfExpr' :: (TransformName n r)  => HsExpr (GhcPass n) -> Trf (AST.UExpr (Dom (GhcPass r)) RangeStage)
trfCmd' :: (TransformName n r)  => HsCmd (GhcPass n) -> Trf (AST.UCmd (Dom (GhcPass r)) RangeStage)