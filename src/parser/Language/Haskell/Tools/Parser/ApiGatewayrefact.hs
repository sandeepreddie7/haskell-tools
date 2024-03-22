
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

transformsRefact :: String -> String -> IO ()
transformsRefact modulePath moduleName = do
    moduleAST <- moduleParser modulePath moduleName
    let allRequestType = nub $ mapMaybe (getAllRequestType) (moduleAST ^? biplateRef)
    transAST <- moduleParser modulePath "Euler.API.Gateway.Gateway.Adyen.Transforms.Transaction"
    flowAST <- moduleParser modulePath "Euler.API.Gateway.Gateway.Adyen.Flow.CaptureVoid"
    routesAST <- moduleParser modulePath "Euler.API.Gateway.Gateway.Adyen.Routes"
    let allFunSigns = concat $ nub $ mapMaybe (getAllTypeSignature allRequestType) (transAST ^? biplateRef)
    let allFunsOfRoutes = mapMaybe (\x -> traverseOverSigBind x ) (routesAST ^? biplateRef)
    let allFunsNamesOfFlows = mapMaybe (\x -> traverseOverSigBindWithCheck allFunsOfRoutes x ) (flowAST ^? biplateRef)
    newAST <- mapM (getAllTransformBinds HM.empty allFunSigns allRequestType flowAST) (flowAST ^? biplateRef)
    let neededHM = HM.unions $ HM.unions <$> fst <$> newAST
    -- newASTFlows <- (!~) (biplateRef) (getAllRoutesValBinds allFunsOfRoutes allFunsNamesOfFlows) (flowAST)
    modifyTransAst <- (!~) (biplateRef) (changeFunSig neededHM allRequestType) (transAST)
    let allTrans = HM.keys neededHM
    modifyFlowAst <- (!~) (biplateRef) (changeFlowFun allTrans) (flowAST)
    newASTFlows <- (!~) (biplateRef) (getAllRoutesValBinds allFunsOfRoutes allFunsNamesOfFlows) (modifyFlowAst)
    print allFunsOfRoutes
    print allFunSigns
    print allFunsNamesOfFlows
    writeFile "modified" (show (neededHM))
    writeFile "modified.hs" (prettyPrint (modifyTransAst))
    writeFile "modifiedNew.hs" (prettyPrint (modifyFlowAst))
    writeFile "modifiedHandle.hs" (prettyPrint (newASTFlows))

addTypeSigArgs :: Ann UType (Dom GhcPs) SrcTemplateStage -> IO (Ann UType (Dom GhcPs) SrcTemplateStage)
addTypeSigArgs expr@(Ann _ (UTyFun ex1 ex2)) = do
    let newType1 = mkVarTypeForRanged $ mkName' "_"
    let newType2 = mkVarTypeForRanged $ mkName' "_"
    let newType3 = mkVarTypeForRanged $ mkName' "_"
    let newType4 = mkVarTypeForRanged $ mkName' "_"
    let newType5 = mkVarTypeForRanged $ mkName' "_"
        combinedType = mkFunctionTypeForRanged (mkFunctionTypeForRanged (mkFunctionTypeForRanged (mkFunctionTypeForRanged newType1 newType2) newType3) newType4) newType5
        combinedTypeWithOrg = mkFunctionTypeForRanged combinedType ex2
        completeType = mkFunctionTypeForRanged ex1 combinedTypeWithOrg
    pure completeType
addTypeSigArgs expr = pure expr

getAllRoutesValBinds :: [String] -> [String] -> Ann UDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UDecl (Dom GhcPs) SrcTemplateStage)
getAllRoutesValBinds allFunsOfRoutes allFunsNamesOfFlows expr@(Ann _ (UValueBinding (FunctionBind' ex))) = do
    newAST <- (!~) (biplateRef) (getAllRoutesBinds allFunsOfRoutes) (expr)
    pure newAST
getAllRoutesValBinds allFunsOfRoutes allFunsNamesOfFlows expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature (AnnListG _ names) typeArg)))) = do
    args <- (!~) (biplateRef) (addTypeSigArgs) (typeArg)
    pure $ if (head $ mapMaybe getNamePart names) `elem` allFunsNamesOfFlows then mkTypeSigDeclForRanged $ mkTypeSignatureForRanged (mkName' ("handleResponse" ++ (head $ mapMaybe getNamePart names))) args else expr

getAllRoutesValBinds _ _ expr = pure expr


getAllRoutesBinds :: [String] -> Ann UMatch (Dom GhcPs) SrcTemplateStage -> IO (Ann UMatch (Dom GhcPs) SrcTemplateStage)
getAllRoutesBinds allFunsOfRoutes expr@(Ann _ (UMatch lhs@(Ann _ (UNormalLhs lhsName args)) (Ann _ (UUnguardedRhs (Ann _ (UDo _ (AnnListG _ stmts))))) (AnnMaybeG _ binds) )) = do
    let allBindsWithRoutes = mapMaybe (\x -> getAllBindsWithRoutes allFunsOfRoutes x ) (expr ^? biplateRef)
    -- print allBindsWithRoutes
    let handlerespCase = filter (getHandleRspCase (concat allBindsWithRoutes) ) (expr ^? biplateRef)
    -- print (handlerespCase, length stmts)
    if not $ null $ concat $ allBindsWithRoutes then
      pure $ mkMatchForRanged (mkMatchLhs' (mkName' $ "handleResponse" ++ (fromMaybe "" $ getNamePart lhsName)) ([mkVarPat' $ mkName' "request", mkVarPat' $ mkName' "vPayload", mkVarPat' $ mkName' "accountDetails",mkVarPat' $ mkName' "gwRequest", mkVarPat' $ mkName' "authPayload",mkVarPat' $ mkName' $ head $ concat allBindsWithRoutes ])) (mkUnguardedRhs' (mkDoBlock' handlerespCase)) binds
      else pure expr
getAllRoutesBinds _ expr = pure expr

getAllBindsWithRoutes :: [String] -> Ann UStmt (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getAllBindsWithRoutes allRoutesFun expr@(Ann _ (UBindStmt stm rhs)) = do
    let allNames = mapMaybe getNamePart (expr ^? biplateRef)
    let bindNames = fromMaybe [] $ getPatternName' stm
    if any (\x -> x `elem` allRoutesFun) allNames then Just bindNames else Nothing
getAllBindsWithRoutes _ _ = Nothing


getHandleRspCase :: [String] -> Ann UStmt (Dom GhcPs) SrcTemplateStage -> Bool
getHandleRspCase allRoutesCall expr@(Ann _ (UExprStmt (Ann _ (UCase casePat (AnnListG _ pats))))) = do
    let caseName = mapMaybe getNamePart (casePat ^? biplateRef)
    any (\x -> x `elem` allRoutesCall) caseName
getHandleRspCase allRoutesCall expr@(Ann _ (UExprStmt (Ann _ (UApp _ (Ann _ (UCase casePat (AnnListG _ pats))))))) = do
    let caseName = mapMaybe getNamePart (casePat ^? biplateRef)
    any (\x -> x `elem` allRoutesCall) caseName
getHandleRspCase _ expr = False

-- handleResponseRtefact :: 
traverseOverSigBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe String
traverseOverSigBind expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature (AnnListG _ names) _)))) = getNamePart $ head names
traverseOverSigBind _ = Nothing

traverseOverSigBindWithCheck :: [String] -> Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe String
traverseOverSigBindWithCheck allRoutesFun expr@(Ann _ (UValueBinding (FunctionBind' ex))) = if any (\x -> x `elem` allRoutesFun ) $ mapMaybe  getNamePart (ex ^? biplateRef) then Just $ head $ mapMaybe  getNamePart (ex ^? biplateRef) else Nothing
traverseOverSigBindWithCheck _ _ = Nothing

-- changeDot :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> IO (Ann UExpr (Dom GhcPs) SrcTemplateStage)
-- changeDot expr@(Ann _ (UInfixApp ex1 (Ann _ (UNormalOp _)) ex2)) = pure $ mkParenForRanged expr
-- changeDot expr = pure expr

getAllTypeSignature :: [String] -> Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getAllTypeSignature reqTypes expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature (AnnListG _ names) typeArg)))) = do
    let allArgs = mapMaybe (getAllTypeArgs) (typeArg ^? biplateRef)
    if (last allArgs) `elem` reqTypes then Just (mapMaybe getNamePart names) else Nothing
getAllTypeSignature _ _ = Nothing

getAllTypeArgs :: Ann UType (Dom GhcPs) SrcTemplateStage -> Maybe String
getAllTypeArgs expr@(Ann _ (UTyVar (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))))) = Just ex
getAllTypeArgs _ = Nothing

getAllRequestType :: Ann UType (Dom GhcPs) SrcTemplateStage -> Maybe String
getAllRequestType expr@(Ann _ (UTyApp (Ann _ (UTyApp (Ann _ (UTyVar (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart "ReqBody")))))))) _)) (Ann _ (UTyVar (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))))) )) = Just ex
getAllRequestType _ = Nothing

getAllTransformBinds :: _ -> [String] -> [String] -> _ -> Ann UMatch (Dom GhcPs) SrcTemplateStage -> IO (_, Ann UExpr (Dom GhcPs) SrcTemplateStage)
getAllTransformBinds hm filList allRequestType flowAST expr@((Ann _ (UMatch lhs@(Ann _ (UNormalLhs lhsName args)) (Ann _ (UUnguardedRhs (Ann _ (UDo _ (AnnListG _ stmts))))) (AnnMaybeG _ binds) ))) = do
    -- print ("hello" ++ show bindNames)
    -- let names = mapMaybe getNamePart (stmts ^? biplateRef)
    val <- mapM (changeFun hm filList allRequestType stmts binds) stmts
    -- print ("eswar2" ++ show allNeeded)
    pure $ (fst <$> val, mkDoBlock' $ snd <$> val)
    --    filter (drop 1 bindNames)
    -- Just ((head $ head bindNames),if any (\x -> x `elem` filList) names then (drop 1 bindNames) else [])
getAllTransformBinds hm _ _ _ expr = pure ([hm],mkVar' $ mkName' "")

filterUMatches :: [String] -> Ann ULocalBinds (Dom GhcPs) SrcTemplateStage -> ([Ann ULocalBind (Dom GhcPs) SrcTemplateStage])
filterUMatches allBinds expr@((Ann _ (ULocalBinds (AnnListG _ matches)))) = do
    let allMatches = filter (isUsed allBinds) matches
    (allMatches)
filterUMatches _ expr = []

changeFun :: _ -> [String] -> [String] -> _ -> _ -> Ann UStmt (Dom GhcPs) SrcTemplateStage -> IO (_,Ann UStmt (Dom GhcPs) SrcTemplateStage)
changeFun hm funList allRequestType dostms binds expr@(Ann _ (UBindStmt stm rhs)) = do
    let bindNames = fromMaybe [[""]] $ getAllExprNamePart (head $ rhs ^? biplateRef)
        getAllFunsInBinds = concat $ filter (\x -> length x == 1) (drop 1 bindNames)
    let allNeeded = filter (isValid getAllFunsInBinds) dostms
        allBinds = maybe [] (filterUMatches getAllFunsInBinds) binds
    let allNames = map (\eachVal -> foldl' (\acc val -> if acc == "Just" then acc <> " " <> val else acc <> "." <> val) (head eachVal) (tail eachVal) ) (drop 1 bindNames)
    let neededHM = HM.insert (head $ head bindNames) ((allNeeded,allBinds),allNames) hm
    -- modifyTransAst <- (!~) (biplateRef) (changeFunSig (head $ head bindNames) allRequestType allNeeded allNames) (ast)
    -- writeFile "modifiedTrans" (prettyPrint modifyTransAst)
    print ("bindNames", bindNames)
    pure $ if any (\x -> x `elem` funList) (head bindNames) then (neededHM , mkBindStmtForRanged stm (mkAppForGhcPs (mkVar' $ mkName' (head $ head bindNames)) (mkVar' $ mkName' "request") )) else (neededHM,expr)
changeFun hm funList allRequestType dostms binds expr@(Ann _ (ULetStmt (AnnListG _ stms))) = do
    val <- mapM (getAllStmts hm funList allRequestType dostms binds) stms
    -- let hmUnion = foldl'
    pure (HM.unions $ fst <$> val,mkLetStmt' $ snd <$> val)
changeFun hm _  _ _ _ expr = pure (hm,expr)

changeFlowFun :: [String] -> Ann UStmt (Dom GhcPs) SrcTemplateStage -> IO (Ann UStmt (Dom GhcPs) SrcTemplateStage)
changeFlowFun funList expr@(Ann _ (UBindStmt stm rhs)) = do
    let bindNames = fromMaybe [[""]] $ getAllExprNamePart (head $ rhs ^? biplateRef)
    pure $ if any (\x -> x `elem` funList) (head bindNames) then ( mkBindStmtForRanged stm (mkAppForGhcPs (mkVar' $ mkName' (head $ head bindNames)) (mkVar' $ mkName' "request") )) else (expr)
changeFlowFun funList expr@(Ann _ (ULetStmt (AnnListG _ stms))) = do
    val <- mapM (getAllStmtsFlow funList) stms
    -- let hmUnion = foldl'
    pure (mkLetStmt' val)
changeFlowFun _ expr = pure (expr)


getAllStmtsFlow :: [String] -> Ann (ULocalBind) (Dom GhcPs) SrcTemplateStage -> IO (Ann (ULocalBind) (Dom GhcPs) SrcTemplateStage)
getAllStmtsFlow funList expr@(Ann _ (ULocalValBind (Ann _ (USimpleBind pat rhs (AnnMaybeG _ binds))))) = do
    let bindNames = fromMaybe [[""]] $ getAllExprNamePart (head $ rhs ^? biplateRef)
    pure $ if any (\x -> x `elem` funList) (head bindNames) then (mkLocalValBind' $ mkSimpleBinds pat (mkUnguardedRhs' $ mkAppForGhcPs (mkVar' $ mkName' (head $ head bindNames)) (mkVar' $ mkName' "request") ) (binds) ) else (expr)
getAllStmtsFlow _ expr = trace "EEE" $ pure (expr)

getAllStmts :: _ -> [String] -> [String] -> _ -> _ -> Ann (ULocalBind) (Dom GhcPs) SrcTemplateStage -> IO (_, Ann (ULocalBind) (Dom GhcPs) SrcTemplateStage)
getAllStmts hm funList allRequestType dostms binds expr@(Ann _ (ULocalValBind (Ann _ (USimpleBind pat rhs (AnnMaybeG _ _))))) = do
    let bindNames = fromMaybe [[""]] $ getAllExprNamePart (head $ rhs ^? biplateRef)
    print ("bindNamesLet", funList, bindNames)
    let getAllFunsInBinds = concat $ filter (\x -> length x == 1) (drop 1 bindNames)
    let allNames = map (\eachVal -> foldl' (\acc val -> if acc == "Just" then acc <> " " <> val else acc <> "." <> val) (head eachVal) (tail eachVal) ) (drop 1 bindNames)
    print ("getAllFunsInBinds", getAllFunsInBinds)
    let allNames = map (\eachVal -> foldl' (\acc val -> if acc == "Just" then acc <> " " <> val else acc <> "." <> val) (head eachVal) (tail eachVal) ) (drop 1 bindNames)
    let allNeeded = filter (isValid getAllFunsInBinds) dostms
        allBinds = maybe [] (filterUMatches getAllFunsInBinds) binds
    print ("allNeeded", allNeeded)
    let neededHM = HM.insert (head $ head bindNames) ((allNeeded,allBinds),allNames) hm
    -- modifyTransAst <- (!~) (biplateRef) (changeFunSig (head $ head bindNames) allRequestType allNeeded allNames) (ast)
    -- writeFile "modifiedTrans" (prettyPrint modifyTransAst)
    pure $ if any (\x -> x `elem` funList) (head bindNames) then (neededHM,mkLocalValBind' $ mkSimpleBinds pat (mkUnguardedRhs' $ mkAppForGhcPs (mkVar' $ mkName' (head $ head bindNames)) (mkVar' $ mkName' "request") ) (binds) ) else (neededHM,expr)
getAllStmts hm _ _ _ _ expr = trace "EEE" $ pure (hm,expr)

changeFunSig :: _ -> [String] -> Ann UDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UDecl (Dom GhcPs) SrcTemplateStage)
changeFunSig hm allRequestType expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature (AnnListG _ names) typeArg)))) = do
    let funNames = HM.keys hm
    if any (\x -> x `elem` funNames) (mapMaybe getNamePart names) then do
        let name = head $ mapMaybe getNamePart names
        let allNames = mapMaybe getNamePart (typeArg ^? biplateRef)
        let typeName = concat $ filter (\x -> x `elem` allRequestType) allNames
        pure $ mkTypeSigDeclForRanged $ mkTypeSignatureForRanged (mkName' name) (mkFunctionTypeForRanged (mkFunctionTypeForRanged (mkFunctionTypeForRanged (mkVarTypeForRanged $ mkName' "_") (mkVarTypeForRanged $ mkName' "_")) (mkVarTypeForRanged $ mkName' "_")) (mkVarTypeForRanged $ mkName' ("L.Flow " ++ typeName)))
    else pure expr
changeFunSig hm allRequestType expr@(Ann _ (UValueBinding exp@(FunctionBind' (ex)))) = do
    let funNames = HM.keys hm
    if any (\x -> x `elem` funNames) (mapMaybe getNamePart (ex ^? biplateRef)) then do
        let name = head $ mapMaybe getNamePart (ex ^? biplateRef)
        case HM.lookup name hm of
            Just ((stmts,allBinds),allNames) -> do
                changedTypeArgs <- (!~) (biplateRef) (changeArgs) (expr)
                let allArgs = head $ mapMaybe (getAllArgs) (expr ^? biplateRef)
                print ("allArgs",allArgs)
                let mappings = zip allArgs allNames
                let newArgsStmts = mkLetStmt' $ map (\(arg,val) -> mkLocalValBind' $  mkSimpleBind'' (mkName' arg) (mkVar' $ mkName' val)) $ filter (\(x,y) -> x /= y) mappings
                print ("newArgsStmts", newArgsStmts, stmts)
                val <- (!~) (biplateRef) (addStmts (stmts ++ [newArgsStmts])) (changedTypeArgs)
                valBinds <- (!~) (biplateRef) (addBindsTrans (allBinds)) (val)
                print ("newArgsStmtsVal", prettyPrint val)
                pure valBinds
            Nothing -> pure expr
    else do
        -- print ("ELse", funName, mapMaybe getNamePart (ex ^? biplateRef))
        pure expr
changeFunSig _ _ expr = do
    print expr
    pure expr

addBindsTrans :: _ -> Ann UMatch (Dom GhcPs) SrcTemplateStage -> IO (Ann UMatch (Dom GhcPs) SrcTemplateStage)
addBindsTrans allBinds expr@(Ann _ (UMatch lhs rhs (AnnMaybeG _ x@(Just (Ann _ (ULocalBinds (AnnListG _ matches)))))) ) = do
      pure $ mkMatchForRanged lhs rhs (Just $ mkLocalBindsForRanged (matches ++ allBinds))
addBindsTrans allBinds expr@(Ann _ (UMatch lhs rhs (AnnMaybeG _ Nothing)) ) = do
      pure $ mkMatchForRanged lhs rhs (Just $ mkLocalBindsForRanged (allBinds))
addBindsTrans _ expr = pure expr

addStmts :: _ -> Ann UMatch (Dom GhcPs) SrcTemplateStage -> IO (Ann UMatch (Dom GhcPs) SrcTemplateStage)
addStmts allStmts expr@(Ann _ (UMatch lhs (Ann _ (UUnguardedRhs (Ann _ (UDo val (AnnListG _ stmts))))) (AnnMaybeG _ binds)) ) = pure $ mkMatchForRanged lhs (mkUnguardedRhs' $ mkDoBlock' (allStmts ++ stmts)) binds
addStmts allStmts expr@(Ann _ (UMatch lhs (Ann _ (UUnguardedRhs ex)) (AnnMaybeG _ binds)) ) = pure $ mkMatchForRanged lhs (mkUnguardedRhs' $ mkDoBlock' (allStmts ++ ([mkExprStmtForRanged ex]))) binds
addStmts _ expr = pure expr

getAllArgs :: Ann UMatchLhs (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getAllArgs expr@(Ann _ (UNormalLhs ex1 (AnnListG _ pats))) = do
    let allPats = (mapMaybe getPatternName' pats)
    Just $ concat allPats
getAllArgs expr = Nothing

changeArgs :: Ann UMatchLhs (Dom GhcPs) SrcTemplateStage -> IO (Ann UMatchLhs (Dom GhcPs) SrcTemplateStage)
changeArgs expr@(Ann _ (UNormalLhs ex1 (AnnListG _ pats))) = do
    let allpats = (mapMaybe getPatternName' pats)
    let pat1 = [mkVarPat' $ mkName' "request", mkVarPat' $ mkName' "accountDetails", mkVarPat' $ mkName' "validationPayload"]
    pure $ mkMatchLhs' ex1 pat1
changeArgs expr = pure expr

getAllExprNamePart :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> Maybe [[String]]
getAllExprNamePart expr@(Ann _ (UInfixApp lhs op (Ann _ (UVar ex)))) = do
    let namePart = (mapMaybe getNamePart (lhs ^? biplateRef))
    Just $ ((fromMaybe [] $ getAllExprNamePart lhs) ++ [[last namePart] ++ mapMaybe getNamePart [ex]])
getAllExprNamePart expr@(Ann _ (UInfixApp lhs op rhs)) = do
    let namePart = (mapMaybe getNamePart (lhs ^? biplateRef))
    let namePartRhs = (mapMaybe getNamePart (rhs ^? biplateRef))
    Just ((fromMaybe [] $ getAllExprNamePart lhs) ++ (fromMaybe [] $ getAllExprNamePart rhs) ++ [[last namePart] ++ [head namePartRhs]])
getAllExprNamePart expr@(Ann _ (UParen ex)) = Just $ [mapMaybe getNamePart (ex ^? biplateRef)]
getAllExprNamePart expr@(Ann _ (UApp lhs rhs)) = Just ((fromMaybe [] $ getAllExprNamePart lhs) ++ (fromMaybe [] $ getAllExprNamePart rhs))
getAllExprNamePart expr@(Ann _ (UVar ex)) = Just $ [mapMaybe getNamePart [ex]]
getAllExprNamePart expr = Nothing

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
isValid _ _ = False

isUsed :: [String] -> Ann ULocalBind (Dom GhcPs) SrcTemplateStage -> Bool
isUsed allFuns expr@(Ann _ (ULocalValBind (Ann _ (USimpleBind pat _ _)))) =
    let bindNames = fromMaybe [] $ getPatternName' pat
    in any (\x -> x `elem` (allFuns)) bindNames
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