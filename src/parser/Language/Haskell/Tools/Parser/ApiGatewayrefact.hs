
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}

module Language.Haskell.Tools.Parser.ApiGatewayrefact where

import Language.Haskell.Tools.Refactor as Refactor hiding (LambdaCase)
import Language.Haskell.Tools.Refactor.Utils.Extensions
import Language.Haskell.Tools.Rewrite.Match.Binds
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar
import GHC hiding (Name, mkModuleName)
import Module as GHC hiding (mkModuleName)
import InstEnv as GHC
import Unify as GHC
import Type as GHC
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
import Data.Char (isAlphaNum, toLower)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List
import qualified Data.Map.Strict as SMap (empty, toList)

import Debug.Trace (trace, traceShowId)
import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances
import Outputable hiding ((<>))
import Language.Haskell.Tools.PrettyPrint

import Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.BackendGHC.GHCUtils (occName, fromSrcText)
import Language.Haskell.Tools.BackendGHC.Names
import Language.Haskell.Tools.BackendGHC.Patterns (trfPattern)
import Language.Haskell.Tools.BackendGHC.Types (trfType)

import Language.Haskell.Tools.AST 
import qualified Language.Haskell.Tools.AST as AST

import Control.Monad.IO.Class

import Language.Haskell.Tools.Refactor
import Control.Monad.IO.Class
import Language.Haskell.Tools.Parser.ParseModule hiding (getFunctionName)
import GHC.Paths ( libdir )
import Data.Generics.Uniplate.Data ()
import  Language.Haskell.Tools.Rewrite.Match.Exprs
import qualified Data.HashMap.Strict as HM
import System.IO
import Control.Monad
import qualified Data.Aeson as A
import Data.List.Extra (replace, foldl, splitOn, stripSuffix)
import System.Directory
import System.IO.Unsafe
import Language.Haskell.Tools.Refactor as HT
import qualified Language.Haskell.Tools.Parser.FlowChange as Fl

import Language.Haskell.Tools.AST.Representation.Binds (ULocalBind)
import Language.Haskell.Tools.Parser.GetFunctionBranching (parseImportsAndGetModule, parseImportsAndGetQualModule)
import Language.Haskell.Tools.Parser.FlowChange  (parseModsAndGetFuns)

validateRefact :: String -> String -> IO ()
validateRefact modulePath moduleName = do
    moduleAST <- moduleParser modulePath moduleName
    importHMQAll <- foldM (\acc x -> parseModsAndGetFuns x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])

    print (importHMQAll)
    -- allFuns <- Fl.getModFunctionList moduleAST "Sample"
    -- print allFuns

validationRefactor :: String -> String -> IO ()
validationRefactor modulePath moduleName = do
    moduleAST <- moduleParser modulePath moduleName
    allFunsSigs <- getAllFunSigs
    let allFunsTobeModfied = concat $ mapMaybe (getAllSigsWithName allFunsSigs) (moduleAST ^? biplateRef)
    newAST <- (!~) (biplateRef) (traverseOverUValBind allFunsTobeModfied) (moduleAST)
    writeFile "modified" (prettyPrint newAST)

traverseOverUValBind :: [String] -> Ann UDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UDecl (Dom GhcPs) SrcTemplateStage)
traverseOverUValBind modFunList expr@(Ann _ (UValueBinding exp@(FunctionBind' (ex)))) = do
    let !funName = mapMaybe (getFunctionNameFromValBind) ((ex) ^? biplateRef)
    if head funName `elem` modFunList then do
        newAST <- (!~) (biplateRef) (gatAllCases) (expr)
        let allCaseVals = concat $ mapMaybe gatAllCasesName (newAST ^? biplateRef)
        modAst <- (!~) (biplateRef) (filterUnWantedBinds allCaseVals) (newAST)
        modAstFIlter <- (!~) (biplateRef) (filterUMatchLocal) (modAst)
        changeFunName <-  (!~) (biplateRef) (filterUMatchLocal) (modAstFIlter)
        -- pure expr
        pure modAstFIlter
    else pure expr
traverseOverUValBind modFunList expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature (AnnListG _ names) ex@(Ann _ (UTyFun (Ann _ (UTyVar name1)) (Ann _ (UTyApp (Ann _ (UTyVar _ )) (Ann _ (UTyVar name2)))))))))) =
    pure $ if (head $ mapMaybe getNamePart names) `elem` modFunList then mkTypeSigDeclForRanged $ mkTypeSignatureForRanged (mkName' ("validateRequest" ++ (head $ mapMaybe getNamePart names))) ex else expr

-- traverseOverUValBind expr@(Ann _ (UValueBinding (SimpleBind' pat rhs var))) = do
--    new
traverseOverUValBind _ expr = pure expr

getFunctionNameFromValBind :: Ann UMatchLhs (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionNameFromValBind expr@(Ann _ (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) ex1)) = Just ex
getFunctionNameFromValBind _ = Nothing

getAllFunSigs = do
    moduleAST <- moduleParser "/home/chaitanya/Desktop/work/euler-api-gateway/src/" "Euler.API.Gateway.Gateway.Common"
    pure $ mapMaybe getAllSigs (moduleAST ^? biplateRef)


-- UTypeSigDecl {_declTypeSig = UTypeSignature {_tsName = [UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "extractAuthenticationWebhook"}}}],
-- _tsType = UTyFun {_typeParam = UTyVar {_typeName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [UNamePart {_simpleNameStr = "API"}], _unqualifiedName = UNamePart {_simpleNameStr = "AuthenticationWebhookRequest"}}}},
-- _typeResult = UTyApp {_typeCon = UTyVar {_typeName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [UNamePart {_simpleNameStr = "L"}], _unqualifiedName = UNamePart {_simpleNameStr = "Flow"}}}}, _typeArg = UTyVar {_typeName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [UNamePart {_simpleNameStr = "API"}], _unqualifiedName = UNamePart {_simpleNameStr = "AuthenticationWebhookResponse"}}}}}}}}

getAllSigs :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, String)
getAllSigs expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature _ (Ann _ (UTyFun (Ann _ (UTyVar name1)) (Ann _ (UTyApp (Ann _ (UTyVar _ )) (Ann _ (UTyVar name2)))))))))) =
    Just (fromMaybe "" $ getNamePart name1, fromMaybe "" $ getNamePart name2)
getAllSigs expr =  Nothing

getAllSigsWithName :: [(String, String)] -> Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getAllSigsWithName funSigList expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature (AnnListG _ names) (Ann _ (UTyFun (Ann _ (UTyVar name1)) (Ann _ (UTyApp (Ann _ (UTyVar _ )) (Ann _ (UTyVar name2)))))))))) =
    if (fromMaybe "" $ getNamePart name1, fromMaybe "" $ getNamePart name2) `elem` funSigList then Just (mapMaybe getNamePart names) else Nothing
getAllSigsWithName _ expr =  Nothing


filterUMatchLocal :: Ann UMatch (Dom GhcPs) SrcTemplateStage -> IO (Ann UMatch (Dom GhcPs) SrcTemplateStage)
filterUMatchLocal expr@(Ann _ (UMatch lhs rhs (AnnMaybeG _ (Just (Ann _ (ULocalBinds (AnnListG _ matches))))))) = do
    let allNames = mapMaybe getNamePart (rhs ^? biplateRef)
    let allMatches = filter (isUsed allNames) matches
        lhsMod = modifyName lhs
    pure $ mkMatchForRanged lhsMod rhs (Just $ mkLocalBindsForRanged (allMatches))
filterUMatchLocal expr = pure expr

modifyName :: Ann UMatchLhs (Dom GhcPs) SrcTemplateStage -> (Ann UMatchLhs (Dom GhcPs) SrcTemplateStage)
modifyName expr@(Ann _ (UNormalLhs name (AnnListG _ args))) = mkMatchLhs' ((mkName' ("validateRequest" ++( fromMaybe "" $ getNamePart name))) ) args

filterUnWantedBinds :: [String] -> Ann UExpr (Dom GhcPs) SrcTemplateStage -> IO (Ann UExpr (Dom GhcPs) SrcTemplateStage)
filterUnWantedBinds filList expr@(Ann _ (UDo _ (AnnListG _ stmts))) = pure $ mkDoBlock' $ filter (isValid filList) stmts
filterUnWantedBinds filList expr = pure expr

isValid :: [String] -> Ann UStmt (Dom GhcPs) SrcTemplateStage -> Bool
isValid filList expr@(Ann _ (UBindStmt stm _)) = do
    let bindNames = fromMaybe [] $ getPatternName' stm
    any (\x -> x `elem` filList) bindNames
isValid _ _ = True

isUsed :: [String] -> Ann ULocalBind (Dom GhcPs) SrcTemplateStage -> Bool
isUsed allFuns expr@(Ann _ (ULocalValBind (Ann _ (USimpleBind pat _ _)))) =
    let bindNames = fromMaybe [] $ getPatternName' pat
    in any (\x -> x `elem` (trace ("funcheck" ++ show allFuns ++ show x) allFuns)) bindNames
isUsed _ _ = True

gatAllCases :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> IO (Ann UExpr (Dom GhcPs) SrcTemplateStage) 
gatAllCases expr@(Ann _ (UCase casePat (AnnListG _ pats))) = do
    let pattens = map (\ex@(Ann _ (UAlt cases rhs _)) -> if isRoutes rhs then mkAltForRanged cases (mkCaseRhsForRanged (mkVar' $ mkName'"undefined")) Nothing else ex) pats -- $ filter (\(Ann _ (UAlt cases rhs _)) -> isRoutes rhs || isLeftCase cases ) pats
    pure $ mkCaseForRanged casePat pattens
gatAllCases expr = pure expr

isLeftCase pat = maybe False (\x -> any (== "Left") x) $ getPatternName' pat

getPatternName' :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getPatternName' expr = Just $ mapMaybe getNamePart (expr ^? biplateRef)

gatAllCasesName :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> Maybe [String]
gatAllCasesName expr@(Ann _ (UCase casePat (AnnListG _ pats))) = do
    let alCaseNames = mapMaybe getNamePart (casePat ^? biplateRef)
    if null alCaseNames then Nothing else Just alCaseNames
gatAllCasesName expr = Nothing

isRoutes :: Ann UCaseRhs (Dom GhcPs) SrcTemplateStage -> Bool
isRoutes rhs = "Routes" `isInfixOf`  (show rhs)


getNamePart :: Ann UName (Dom GhcPs) SrcTemplateStage -> Maybe String
getNamePart expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = Just ex
getNamePart expr = Nothing