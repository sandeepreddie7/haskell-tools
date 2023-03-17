{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds, LiberalTypeSynonyms, DataKinds #-}

-- | Functions that convert the basic elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Names where

import Control.Monad.Reader ((=<<), asks)
import Data.Char (isAlphaNum)
import Data.List.Split (splitOn)
import Data.Data (Data)

import FastString as GHC (FastString, unpackFS)
import GHC.Hs.ImpExp as GHC
import Name as GHC (isSymOcc, occNameString)
import qualified Name as GHC (Name)
import OccName as GHC (HasOccName)
import RdrName as GHC (RdrName)
import SrcLoc as GHC
import qualified Id  as GHC (Id)
import GHC.Hs.Lit as GHC (OverLitVal(..), HsLit(..))
import Outputable (Outputable)
import GHC.Hs.Types as GHC
import GHC

import Language.Haskell.Tools.AST (Ann(..), AnnListG, RangeStage, Dom)
import qualified Language.Haskell.Tools.AST as AST

import Language.Haskell.Tools.BackendGHC.GHCUtils
import Language.Haskell.Tools.BackendGHC.Monad
import Language.Haskell.Tools.BackendGHC.Utils

trfOperator :: forall n r . TransformName n r => Located (IdP (GhcPass n)) -> Trf (Ann AST.UOperator (Dom (GhcPass r)) RangeStage)
trfOperator = trfLocNoSema (trfOperator' @n)

trfOperator' :: forall n r . TransformName n r => IdP (GhcPass n) -> Trf (AST.UOperator (Dom (GhcPass r)) RangeStage)
trfOperator' n
  | isSymOcc (occName @n n) = AST.UNormalOp <$> (trfQualifiedNameFocus @n) True n
  | otherwise = AST.UBacktickOp <$> (trfQualifiedNameFocus @n) True n

trfName :: forall n r . TransformName n r => Located (IdP (GhcPass n)) -> Trf (Ann AST.UName (Dom (GhcPass r)) RangeStage)
trfName = trfLocNoSema (trfName' @n)

trfName' :: forall n r . TransformName n r => IdP (GhcPass n) -> Trf (AST.UName (Dom (GhcPass r)) RangeStage)
trfName' n
  | isSymOcc (occName @n n) = (if isSpecKind then AST.UNormalName else AST.UParenName) <$> (trfQualifiedNameFocus @n) isSpecKind n
  | otherwise = AST.UNormalName <$> (trfQualifiedNameFocus @n) False n
  where -- special names that are operators, but appear in name context
    isSpecKind = occNameString (occName @n n) `elem` ["*", "#", "?", "??"]

trfAmbiguousFieldName :: TransformName n r => Located (AmbiguousFieldOcc (GhcPass n)) -> Trf (Ann AST.UName (Dom (GhcPass r)) RangeStage)
trfAmbiguousFieldName (L l af) = trfAmbiguousFieldName' l af

trfAmbiguousFieldName' :: forall n r . TransformName n r => SrcSpan -> AmbiguousFieldOcc (GhcPass n) -> Trf (Ann AST.UName (Dom (GhcPass r)) RangeStage)
trfAmbiguousFieldName' l (Unambiguous pr (L _ rdr)) = annLocNoSema (pure l) $ trfName' @n (fieldOccToId @n rdr pr)
-- no Id transformation is done, so we can basically ignore the postTC value
trfAmbiguousFieldName' _ (Ambiguous _ (L l rdr))
  = annLocNoSema (pure l)
      $ (if (isSymOcc (occName @Parsed rdr)) then AST.UParenName else AST.UNormalName)
          <$> (annLoc (createAmbigousNameInfo rdr l) (pure l) $ AST.nameFromList <$> trfNameStr (isSymOcc (occName @Parsed rdr)) (rdrNameStr rdr))

trfAmbiguousOperator' :: forall n r . TransformName n r => SrcSpan -> AmbiguousFieldOcc (GhcPass n) -> Trf (Ann AST.UOperator (Dom (GhcPass r)) RangeStage)
trfAmbiguousOperator' l (Unambiguous pr (L _ rdr)) = annLocNoSema (pure l) $ trfOperator' @n (fieldOccToId @n rdr pr)
-- no Id transformation is done, so we can basically ignore the postTC value
trfAmbiguousOperator' _ (Ambiguous _ (L l rdr))
  = annLocNoSema (pure l)
      $ (if (isSymOcc (occName @Parsed rdr)) then AST.UNormalOp else AST.UBacktickOp)
          <$> (annLoc (createAmbigousNameInfo rdr l) (pure l) $ AST.nameFromList <$> trfOperatorStr (not $ isSymOcc (occName @Parsed rdr)) (rdrNameStr rdr))

type CorrectPass n = ( Data (HsLit n), Outputable (HsLit n)
                     , Data (HsType n), Outputable (HsType n)
                     , Data (Pat n), Outputable (Pat n)
                     , Data (HsExpr n), Outputable (HsExpr n)
                     , Data (Stmt n (LHsExpr n)), Outputable (Stmt n (LHsExpr n))
                     , Data (Stmt n (LHsCmd n)), Outputable (Stmt n (LHsCmd n))
                     , Data (HsCmd n), Outputable (HsCmd n)
                     , Data (Sig n), Outputable (Sig n)
                     , Data (HsLocalBinds n), Outputable (HsLocalBinds n)
                     , Data (HsBind n), Outputable (HsBind n)
                     , Data (IdP n), Outputable (IdP n)
                     , Data (ConDecl n), Outputable (ConDecl n)
                     , Data (HsDecl n), Outputable (HsDecl n)
                     , Data (HsSplice n), Outputable (HsSplice n)
                     )

type ConvOk n = ( XSigPat n ~ HsWildCardBndrs n (HsImplicitBndrs n (LHsType n))
                , XExprWithTySig n ~ LHsSigWcType n
                , XAppTypeE n ~ LHsWcType n
                , NameOrRdrName (IdP n) ~ IdP n
                )

class (Eq (GhcPass n), CorrectPass (GhcPass n), GHCName n, FromGHCName (IdP (GhcPass n)), HasOccName (IdP (GhcPass n)))
        => TransformableName n where
  correctNameString :: IdP (GhcPass n) -> Trf String
  transformSplice :: HsSplice GhcPs -> Trf (HsSplice (GhcPass n))

instance TransformableName Parsed where
  correctNameString = pure . rdrNameStr
  transformSplice = pure

instance TransformableName Renamed where
  correctNameString n = getOriginalName (rdrName @Renamed n)
  transformSplice = rdrSplice

-- | This class allows us to use the same transformation code for multiple variants of the GHC AST.
-- GHC UName annotated with 'name' can be transformed to our representation with semantic annotations of 'res'.
class (TransformableName name, HsHasName (IdP (GhcPass name)), FromGHCName (IdP (GhcPass res)), Eq (IdP (GhcPass name)), GHCName res, NameOrRdrName (IdP  (GhcPass name)) ~ (IdP (GhcPass name)), XUnambiguous (GhcPass name) ~ XCFieldOcc (GhcPass name))
        => TransformName name res where
  -- | Demote a given name
  transformName :: IdP (GhcPass name) -> IdP (GhcPass res)

instance TransformName Parsed Parsed where transformName = id

instance {-# OVERLAPPABLE #-} (FromGHCName (IdP (GhcPass res)), GHCName res) => TransformName Renamed res where
  transformName = fromGHCName

-- instance {-# OVERLAPPABLE #-} (TransformableName x, HsHasName (IdP (GhcPass x)), FromGHCName (IdP (GhcPass res)), Eq (IdP (GhcPass x)), GHCName res, NameOrRdrName (IdP  (GhcPass x)) ~ (IdP (GhcPass x)), XUnambiguous (GhcPass x) ~ XCFieldOcc (GhcPass x), x ~ NoGhcTcPass name) => TransformName x res where
--   transformName = undefined

trfNameText :: String -> Trf (Ann AST.UName (Dom r) RangeStage)
trfNameText str
  = annContNoSema (AST.UNormalName <$> annLoc (createImplicitNameInfo str) (asks contRange) (AST.nameFromList <$> trfNameStr (isOperatorStr str) str))

trfImplicitName :: HsIPName -> Trf (Ann AST.UName (Dom r) RangeStage)
trfImplicitName (HsIPName fs)
  = let nstr = unpackFS fs
     in do rng <- asks contRange
           let rng' = mkSrcSpan (updateCol (+1) (srcSpanStart rng)) (srcSpanEnd rng)
           annContNoSema (AST.UImplicitName <$> annLoc (createImplicitNameInfo nstr) (pure rng')
                                                  (AST.nameFromList <$> trfNameStr (isOperatorStr nstr) nstr))

isOperatorStr :: String -> Bool
isOperatorStr = any (not . isAlphaNum)

trfQualifiedName :: forall n r . TransformName n r => Bool -> Located (IdP (GhcPass n)) -> Trf (Ann AST.UQualifiedName (Dom (GhcPass r)) RangeStage)
trfQualifiedName isOperator (L l n) = focusOn l $ (trfQualifiedNameFocus @n) isOperator n

trfQualifiedNameFocus :: forall n r . TransformName n r => Bool -> IdP (GhcPass n) -> Trf (Ann AST.UQualifiedName (Dom (GhcPass r)) RangeStage)
trfQualifiedNameFocus isOperator n
  = do rng <- asks contRange
       let rng' = if isOperator == isSymOcc (occName @n n) then rng
                    else mkSrcSpan (updateCol (+1) (srcSpanStart rng)) (updateCol (subtract 1) (srcSpanEnd rng))
       annLoc (createNameInfo (transformName @n @r n)) (pure rng') (trfQualifiedName' @n n)

trfQualifiedName' :: forall n r . TransformName n r => IdP (GhcPass n) -> Trf (AST.UQualifiedName (Dom (GhcPass r)) RangeStage)
trfQualifiedName' n = AST.nameFromList <$> ((if isSymOcc (occName @n n) then trfOperatorStr else trfNameStr) False =<< (correctNameString @n) n)

trfOperatorStr :: Bool -> String -> Trf (AnnListG AST.UNamePart (Dom r) RangeStage)
trfOperatorStr isInParen str = do rng <- correctSpan <$> asks contRange
                                  makeList "." (pure $ srcSpanStart rng)
                                               (pure [Ann (noSemaInfo $ AST.NodeSpan rng) (AST.UNamePart str)])
  where correctSpan sp = if isInParen then mkSrcSpan (updateCol (+1) (srcSpanStart sp))
                                                     (updateCol (subtract 1) (srcSpanEnd sp))
                                      else sp

-- | Creates a qualified name from a name string
trfNameStr :: Bool -> String -> Trf (AnnListG AST.UNamePart (Dom r) RangeStage)
trfNameStr isInBackticks str = makeList "." atTheStart (trfNameStr' str . correct <$> atTheStart)
  where correct = if isInBackticks then updateCol (+1) else id

trfNameStr' :: String -> SrcLoc -> [Ann AST.UNamePart (Dom r) RangeStage]
trfNameStr' str startLoc = fst $
  foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                         in ( r ++ [Ann (noSemaInfo $ AST.NodeSpan (mkSrcSpan loc nextLoc)) (AST.UNamePart np)], advanceAllSrcLoc nextLoc "." ) )
  ([], startLoc) (splitOn "." str)
  where -- | Move the source location according to a string
        advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
        advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
        advanceAllSrcLoc oth _ = oth

trfFastString :: Located FastString -> Trf (Ann AST.UStringNode (Dom (GhcPass r)) RangeStage)
trfFastString = trfLocNoSema $ pure . AST.UStringNode . unpackFS
