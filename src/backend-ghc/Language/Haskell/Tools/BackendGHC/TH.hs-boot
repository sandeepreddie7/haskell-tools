{-# LANGUAGE TypeFamilies, DataKinds #-}
module Language.Haskell.Tools.BackendGHC.TH where

import GHC.Hs.Expr as GHC (HsSplice, HsBracket)
import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..))
import GHC.Hs.Extension

trfQuasiQuotation' :: TransformName n r => HsSplice (GhcPass n) -> Trf (AST.UQuasiQuote (Dom (GhcPass r)) RangeStage)
trfSplice :: (TransformName n r) => HsSplice (GhcPass n) -> Trf (Ann AST.USplice (Dom (GhcPass r)) RangeStage)
trfSplice' :: (TransformName n r) => HsSplice (GhcPass n) -> Trf (AST.USplice (Dom (GhcPass r)) RangeStage)
trfBracket' :: (TransformName n r) => HsBracket (GhcPass n) -> Trf (AST.UBracket (Dom (GhcPass r)) RangeStage)
