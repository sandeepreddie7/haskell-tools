-- | Utility methods for generating parts of the AST for refactorings
{-# LANGUAGE MonoLocalBinds #-}

module Language.Haskell.Tools.Rewrite.Create.Utils where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare (child, opt, list)
import Language.Haskell.Tools.Rewrite.ElementTypes (AnnMaybe, AnnList)
import GHC

fromTemplate :: src -> NodeInfo sema src
fromTemplate = NodeInfo (error "The newly generated AST fragments have no semantic info, but a refactoring tries to use that information.")

emptyList :: AnnList e
emptyList = AnnListG (fromTemplate list) []

emptyList' ::  AnnListG e (Dom GhcPs) SrcTemplateStage
emptyList' = AnnListG (fromTemplate list) []

justVal :: Ann e IdDom SrcTemplateStage -> AnnMaybe e
justVal e = AnnMaybeG (fromTemplate opt) (Just e)

noth :: AnnMaybe e
noth = AnnMaybeG (fromTemplate opt) Nothing

mkAnn :: SpanInfo SrcTemplateStage -> e IdDom SrcTemplateStage -> Ann e IdDom SrcTemplateStage
mkAnn temp = Ann (fromTemplate temp)

mkAnn' :: SpanInfo SrcTemplateStage -> e (Dom GhcPs) SrcTemplateStage -> Ann e (Dom GhcPs) SrcTemplateStage
mkAnn' temp = Ann (fromTemplate temp)

-- | Annotation for a simple wrapper AST node
wrapperAnn :: e IdDom SrcTemplateStage -> Ann e IdDom SrcTemplateStage
wrapperAnn = mkAnn child

-- | Transforms the list of elements to an AnnListG with the given source template.
mkAnnList :: ListInfo SrcTemplateStage -> [Ann e IdDom SrcTemplateStage] -> AnnList e
mkAnnList temp = AnnListG (fromTemplate temp)

mkAnnList' :: ListInfo SrcTemplateStage -> [Ann e (Dom GhcPs) SrcTemplateStage] -> AnnListG e (Dom GhcPs) SrcTemplateStage
mkAnnList' temp = AnnListG (fromTemplate temp)

-- | Transforms the Maybe element to an AnnMaybeG with the given source template.
mkAnnMaybe :: OptionalInfo SrcTemplateStage -> Maybe (Ann e IdDom SrcTemplateStage) -> AnnMaybe e
mkAnnMaybe temp = AnnMaybeG (fromTemplate temp)

mkAnnMaybe' :: OptionalInfo SrcTemplateStage -> Maybe (Ann e (Dom GhcPs) SrcTemplateStage) -> AnnMaybeG e (Dom GhcPs) SrcTemplateStage
mkAnnMaybe' temp = AnnMaybeG (fromTemplate temp)
