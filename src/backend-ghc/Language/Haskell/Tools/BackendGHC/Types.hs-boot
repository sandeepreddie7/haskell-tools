{-# LANGUAGE TypeFamilies, DataKinds #-}
-- | Functions that convert the type-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Types where

import GHC.Hs.Types as GHC (HsType)
import Language.Haskell.Tools.AST as AST (Ann, UType, Dom, RangeStage)
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName)
import GHC.Hs.Extension
import SrcLoc as GHC (Located)
import GHC.Stack (HasCallStack)

trfType :: (TransformName n r, HasCallStack)  => Located (HsType (GhcPass n)) -> Trf (Ann AST.UType (Dom (GhcPass r)) RangeStage)
trfType' :: (TransformName n r, HasCallStack)  => HsType (GhcPass n) -> Trf (AST.UType (Dom (GhcPass r)) RangeStage)
