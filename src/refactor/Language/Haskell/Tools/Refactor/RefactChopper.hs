
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Tools.Refactor.RefactChopper where

import Language.Haskell.Tools.Refactor as Refactor hiding (LambdaCase)
import Language.Haskell.Tools.Refactor.Utils.Extensions
import Language.Haskell.Tools.Rewrite.Match.Binds
-- import Language.Haskell.Tools.AST.Representation.Binds as AST
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar
import GHC hiding (Name, mkModuleName)
import Module as GHC hiding (mkModuleName)
import InstEnv as GHC
import Unify as GHC
import Type as GHC
-- import Name as GHC
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

import Language.Haskell.Tools.AST 
import qualified Language.Haskell.Tools.AST as AST

import Control.Monad.IO.Class

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances
import Control.Monad.IO.Class
import Language.Haskell.Tools.Refactor.ChopperFile
import GHC.Paths ( libdir )
import Data.Generics.Uniplate.Data ()
import  Language.Haskell.Tools.Rewrite.Match.Exprs
import Language.Haskell.Tools.Refactor
import qualified Data.HashMap.Strict as HM
import System.IO
import Control.Monad




-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.


writeBack =
        do
            moduleAST <- moduleParser "/home/chaitanya/Desktop/work/euler-api-txns/euler-x/src-generated/ConfigFramework/Storage/ConfigMeta.hs" "Product.OLTP.Transaction"
            -- (AST.AnnListG annot currentDecl) <- (moduleAST ^? modDecl)

            let !funDepList = mapMaybe (traverseOverUValBind) (moduleAST ^? biplateRef)
            let funList = HM.fromList $ map (\(fun,y) -> (fun, (fst y , nub $ filter (\x -> x `elem` (fst <$> funDepList)) (snd y)))) funDepList
            let resolveDeps = HM.foldlWithKey' (\acc key (x,y) -> HM.insert key (nub $ getAllRelatedFuns x y funList) acc) HM.empty funList
            -- let deciderDeps = maybe [] (id) $ HM.lookup "decider" resolveDeps
            -- writeFile "MetaAST" (show moduleAST)
            -- writeFile "Meta" (show resolveDeps)
            let funDepList = [["handleCardDirectDebitResp", "cardDirectDebitProcess", "generateDynamicQR", "proxyToCall", "initialiseTransaction", "checkPaymentLockingAndInitTxn", "resolveCardToken", "initialiseTransactionWorkflow", "initialiseSplitPaymentTxnFlow", "decider"], ["verifyIsTxnProcessedThroughMandateToNormalTxnFlowAndUpdateMandate"], ["checkAndSetupMandate"], ["isStatusFlowAndCardEligibleForTokenFlow", "verifyAndUpdateTokenForMandateFlowHandler", "updateResponseWithEncryptedGwParams", "updateResponseWithEncryptedGwParamsAndEpgTxnId", "verifyAndUpdateTokenInfoInGwParams"], ["verifyPgMandateIdAndSetMetadataAndMandate"], ["updateMandateStatusAsFailure"], ["checkAndSetupMandateWithType", "checkAndUpdateSplitDetailsBlockWithType", "updateTrackerTablesWithType"], ["initEnrollment", "cmpiLookup", "doCmpiLookup"], ["checkRetargetPossibilityAndUpdateTrackers", "updateTxnOrderPGRAndMandateBasedOnDecision", "updateTxnDetailWithObjRefId", "initiateExecuteMandate"], ["fetchNotificationRecord", "shouldNotifyMandatePriorToExecution", "fetchCustomerAndMandateFromMaybe", "getMAAndMandateRefId", "decideNotificationObjRefId", "createInternalMetadataForMandateWorkflowTxns", "decideMandateExecutionFlowAndProcessTxn", "mandateTxnProcess"], ["startPayment"], ["directOTPTxnProcess"], ["validateTxnExpiry", "getWalletRefIdAndMaybeSfr", "verifyAndUpdateMandateStatus", "getAuthorizationResponseFromPG", "doAuthZAndHandleResp", "viesResponseHandler", "processRedirectTxnHandler", "processOTPTransaction", "viesStartPaymentFlow", "processRedirectTxn"], ["checkAndAddCardToLocker", "shouldUpdateOnlyOrderForRetarget", "checkAndUpdateSecondFactor"], ["generateQRCode", "decideOrderType", "createTxnFlow", "verifyIsMandateRegOrRegDebitAndDecideTypeAndAmount", "postInitialiseTxnWorker", "foreignToString"], ["updateTrackerTables"], ["verifyUpdateAndSendResponseTopUp", "handleWalletDebitResponse", "handleRegisterAndDebit", "failSplitPaymentDirectDebitTxn", "getDirectDebitResponse", "processTopUpResponse", "handleRedirectRespTopUp", "handleResponseTopUp", "walletDirectDebitFlow", "walletDirectDebitProcess"], ["isRespSuccessOrSplit", "processViesResponse", "processResponse", "handleRedirectResp", "handlePayResponse", "handleAmazonPayResponse", "handleStaticUrlResponse"], ["handleCardMotoTxnResponse", "initiateMotoTxnProcess"], ["isCustomWalletTxnValid", "handleCustomWallet"], ["rejectRequestIfAutoCaptureIsFalse", "deleteTransientDataOfThreeDS2ChargedTxn", "handleSubmitOtpResponse", "processOtp", "verifyAndCaptureResponse", "verifyGatewayResponse", "verifyStatusAndCreateOrUpdateWallet", "verifyUpdateAndSendResponse"], ["getInAPPSdkParams", "inAPPUPISdkProcess"], ["isTxnStatusInNonTerminalState", "isTxnProcessedThroughMandatToNormalTxnFlow"], ["processRecurringDebit", "processMandateExecution'", "declineTxnOrderAndUpdatePgrAsUnprocessableEntity", "processMandateExecution"], ["updateTopupTxn", "updTrackerOfTopup"], ["shouldCreateSubscription", "sdkRedirectProcess"], ["consumerFinanceInitiateRedirectionTxnFlow", "redirectTxnProcess"], ["getUpdatedMandateParamsWithPGMandId", "updateMandateParamsWithRegResp", "updateSfWithHsResponse", "sendCollectRequestAndHandleResponse", "sendCollectRequest"], ["checkAndUpdateMobileNumberOnOrd"], ["resendOtp"], ["checkAndUpdateSplitDetailsBlock"]]
            -- !newAST <- liftGhc $ (!~) (biplateRef) (logicFn) (moduleAST)
            let (clusterList :: HM.HashMap String Int) = HM.fromList [("handleCardDirectDebitResp", 0), ("cardDirectDebitProcess", 0), ("generateDynamicQR", 0), ("proxyToCall", 0), ("initialiseTransaction", 0), ("checkPaymentLockingAndInitTxn", 0), ("resolveCardToken", 0), ("initialiseTransactionWorkflow", 0), ("initialiseSplitPaymentTxnFlow", 0), ("decider", 0), ("verifyIsTxnProcessedThroughMandateToNormalTxnFlowAndUpdateMandate", 1), ("checkAndSetupMandate", 2), ("isStatusFlowAndCardEligibleForTokenFlow", 3), ("verifyAndUpdateTokenForMandateFlowHandler", 3), ("updateResponseWithEncryptedGwParams", 3), ("updateResponseWithEncryptedGwParamsAndEpgTxnId", 3), ("verifyAndUpdateTokenInfoInGwParams", 3), ("verifyPgMandateIdAndSetMetadataAndMandate", 4), ("updateMandateStatusAsFailure", 5), ("checkAndSetupMandateWithType", 6), ("checkAndUpdateSplitDetailsBlockWithType", 6), ("updateTrackerTablesWithType", 6), ("initEnrollment", 7), ("cmpiLookup", 7), ("doCmpiLookup", 7), ("checkRetargetPossibilityAndUpdateTrackers", 8), ("updateTxnOrderPGRAndMandateBasedOnDecision", 8), ("updateTxnDetailWithObjRefId", 8), ("initiateExecuteMandate", 8), ("fetchNotificationRecord", 9), ("shouldNotifyMandatePriorToExecution", 9), ("fetchCustomerAndMandateFromMaybe", 9), ("getMAAndMandateRefId", 9), ("decideNotificationObjRefId", 9), ("createInternalMetadataForMandateWorkflowTxns", 9), ("decideMandateExecutionFlowAndProcessTxn", 9), ("mandateTxnProcess", 9), ("startPayment", 10), ("directOTPTxnProcess", 11), ("validateTxnExpiry", 12), ("getWalletRefIdAndMaybeSfr", 12), ("verifyAndUpdateMandateStatus", 12), ("getAuthorizationResponseFromPG", 12), ("doAuthZAndHandleResp", 12), ("viesResponseHandler", 12), ("processRedirectTxnHandler", 12), ("processOTPTransaction", 12), ("viesStartPaymentFlow", 12), ("processRedirectTxn", 12), ("checkAndAddCardToLocker", 13), ("shouldUpdateOnlyOrderForRetarget", 13), ("checkAndUpdateSecondFactor", 13), ("generateQRCode", 14), ("decideOrderType", 14), ("createTxnFlow", 14), ("verifyIsMandateRegOrRegDebitAndDecideTypeAndAmount", 14), ("postInitialiseTxnWorker", 14), ("foreignToString", 14), ("updateTrackerTables", 15), ("verifyUpdateAndSendResponseTopUp", 16), ("handleWalletDebitResponse", 16), ("handleRegisterAndDebit", 16), ("failSplitPaymentDirectDebitTxn", 16), ("getDirectDebitResponse", 16), ("processTopUpResponse", 16), ("handleRedirectRespTopUp", 16), ("handleResponseTopUp", 16), ("walletDirectDebitFlow", 16), ("walletDirectDebitProcess", 16), ("isRespSuccessOrSplit", 17), ("processViesResponse", 17), ("processResponse", 17), ("handleRedirectResp", 17), ("handlePayResponse", 17), ("handleAmazonPayResponse", 17), ("handleStaticUrlResponse", 17), ("handleCardMotoTxnResponse", 18), ("initiateMotoTxnProcess", 18), ("isCustomWalletTxnValid", 19), ("handleCustomWallet", 19), ("rejectRequestIfAutoCaptureIsFalse", 20), ("deleteTransientDataOfThreeDS2ChargedTxn", 20), ("handleSubmitOtpResponse", 20), ("processOtp", 20), ("verifyAndCaptureResponse", 20), ("verifyGatewayResponse", 20), ("verifyStatusAndCreateOrUpdateWallet", 20), ("verifyUpdateAndSendResponse", 20), ("getInAPPSdkParams", 21), ("inAPPUPISdkProcess", 21), ("isTxnStatusInNonTerminalState", 22), ("isTxnProcessedThroughMandatToNormalTxnFlow", 22), ("processRecurringDebit", 23), ("processMandateExecution'", 23), ("declineTxnOrderAndUpdatePgrAsUnprocessableEntity", 23), ("processMandateExecution", 23), ("updateTopupTxn", 24), ("updTrackerOfTopup", 24), ("shouldCreateSubscription", 25), ("sdkRedirectProcess", 25), ("consumerFinanceInitiateRedirectionTxnFlow", 26), ("redirectTxnProcess", 26), ("getUpdatedMandateParamsWithPGMandId", 27), ("updateMandateParamsWithRegResp", 27), ("updateSfWithHsResponse", 27), ("sendCollectRequestAndHandleResponse", 27), ("sendCollectRequest", 27), ("checkAndUpdateMobileNumberOnOrd", 28), ("resendOtp", 29), ("checkAndUpdateSplitDetailsBlock", 30)]
            (AST.AnnListG annot currentDecl) <- moduleAST ^? (modDecl) 
            let newImportsList = map nub $ snd $ foldl' (\(l,acc) fun -> (l+1,acc ++ [foldl' (\(acc) x -> maybe (acc) (\val -> ((fetchNum val clusterList l) ++ acc)) $ HM.lookup x resolveDeps) ([]) fun])) (0,[]) funDepList
            -- print $ newImportsList
            !newModDecl <- mapM (\(x,y) -> do
                                    let decls = filter (\decl -> checkFun decl x) currentDecl
                                        nAST = (.=) modDecl (AST.AnnListG annot decls) moduleAST
                                    impsFil <- ((\(AST.AnnListG annot val) -> pure $ modifyImports y val) =<< (nAST ^? modImports)) --(.=) modDecl (AST.AnnListG annot decls) nAST
                                    pure $ (.=) modImports (AST.AnnListG annot impsFil) nAST
                                ) (zip funDepList newImportsList) 
            let newModDeclForRoot = let decls = filter (\decl -> not $ checkFun decl (concat funDepList)) currentDecl
                                        in (.=) modDecl (AST.AnnListG annot decls) moduleAST
            writeFile "/home/chaitanya/Desktop/work/euler-api-txns/euler-x/src-generated/Product/OLTP/Transaction.hs" (prettyPrint newModDeclForRoot)
            foldM (\acc x -> do
                changedModName <- liftIO $ (!~) (biplateRef) (changeModName (".Transaction" ++ show acc)) (x)
                (writeFile ("/home/chaitanya/Desktop/work/euler-api-txns/euler-x/src-generated/Product/OLTP/Transaction/Transaction" Prelude.<> show acc Prelude.<> ".hs") (prettyPrint changedModName))
                pure $ (acc + 1)) 0 newModDecl
            pure ()
            -- let newFunMod = mapMaybe (traverseOverUValBind) (moduleAST ^? biplateRef)

    where
        modifyImports :: [String] -> [Ann UImportDecl (Dom GhcPs) SrcTemplateStage] -> [Ann UImportDecl (Dom GhcPs) SrcTemplateStage]
        modifyImports imps val =  foldl' (\(acc) x ->  ( acc ++ [mkImportDecl' False False False Nothing (mkModuleName' x) Nothing Nothing])) (val) imps
        fetchNum :: [String] -> HM.HashMap String Int -> Int -> [String]
        fetchNum val clusterList l = foldl' (\(acc) x -> maybe (acc) (\y -> if l == y then acc else (["Product.OLTP.Transaction.Transaction" ++ (show y)] ++ acc)) $ HM.lookup x clusterList) [] val
        getAllRelatedFuns whereDeps funs funList = foldl' (\acc val -> case HM.lookup val funList of
                                                                         Just f -> acc ++ snd f
                                                                         Nothing -> acc) funs whereDeps

        -- getDeciderDeps _ [] visitedFuns = pure []
        -- getDeciderDeps resolveDeps deciderDeps visitedFuns = do
        --     visitedFunList <- readMVar visitedFuns
        --     let allFuns = concatMap (\x -> x :
        --                         if x `elem` visitedFunList 
        --                         then x
        --                         else unsafePerformIO $ (getDeciderDeps resolveDeps (maybe [] id (HM.lookup x resolveDeps)) visitedFuns)) deciderDeps
        --     putMVar visitedFunList allFuns
        --     pure allFuns
-- check :: Decl -> Bool 
-- check (InstanceDecl (InstanceRule _ _ (AppInstanceHead fn _)) _) =
--                                                 let (x :: [String]) = fn ^? biplateRef
--                                                 in not $ any (\x -> x == "HasField") x    
-- check _ = True

-- UApp {_exprFun = UApp {_exprFun = UExplTypeApp {_exprInner = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [UNamePart {_simpleNameStr = "GHC"},UNamePart {_simpleNameStr = "Records"},UNamePart {_simpleNameStr = "Extra"}], _unqualifiedName = UNamePart {_simpleNameStr = "setField"}}}},
--                                                               _exprType = UTyPromoted {_tpPromoted = UPromotedString {_promotedStringValue = "name"}}}, 
--                   _exprArg = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "pm"}}}}}, 
--       _exprArg = ULit {_exprLit = UStringLit {_stringLitValue = "MASTER"}}}

-- UInfixApp {_exprLhs = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "x"}}}},
--            _exprOperator = UNormalOp {_operatorName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "^."}}},
--            _exprRhs = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [UNamePart {_simpleNameStr = "L"}], _unqualifiedName = UNamePart {_simpleNameStr = "_merchantAccountId"}}}}}

-- ULet {_exprFunBind = [ULocalValBind {_localVal = USimpleBind {_valBindPat = UVarPat {_patternName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "dingding"}}}},
--                                                               _valBindRhs = UUnguardedRhs {_rhsExpr = UParen {_exprInner = UApp {_exprFun = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "encodeDecodeTransform"}}}},
--                                                                                                                                  _exprArg = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "scope"}}}}}}},
--                                                               _valBindLocals = Nothing}}],
--       _exprInner = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "dingding"}}}}}

-- UApp {_exprFun = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [UNamePart {_simpleNameStr = "Se"}], _unqualifiedName = UNamePart {_simpleNameStr = "Eq"}}}}, _exprArg = UParen {_exprInner = UApp {_exprFun = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "encodeDecodeTransform"}}}}, _exprArg = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "name"}}}}}}}

-- UApp {_exprFun = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "encodeDecodeTransform"}}}},
--       _exprArg = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "name"}}}}}

changeModName :: String -> Ann UModuleHead (Dom GhcPs) SrcTemplateStage -> IO (Ann UModuleHead (Dom GhcPs) SrcTemplateStage)
changeModName str expr@(Ann y ((UModuleHead (Ann b (UModuleName name)) prag (AnnMaybeG x _)))) = do
   let names = name ++ str
   print names
   let modName = mkModuleName' names
   let modHead = mkModuleHead' modName Nothing Nothing
   pure modHead


checkFun :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> [String] -> Bool
checkFun expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature funName _)))) str = any (== (head $ map getFunctions' $ funName ^? biplateRef)) str
checkFun expr@(Ann _ (UValueBinding (FunctionBind' ex))) str =
    let !funName = map (getFunctionNameFromValBind) ((ex) ^? biplateRef)
    in any (== head funName) str
checkFun _ _ = False

traverseOverUValBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, ([String],[String]))
traverseOverUValBind expr@(Ann _ (UValueBinding (FunctionBind' ex))) = do
    let !funName = map (getFunctionNameFromValBind) ((ex) ^? biplateRef)
    let !funNameMap = (head funName, tail funName)
    let !funDeps = concat $ map (getFunctionsCalledInFunction) (expr ^? biplateRef)
    Just (head funName, (tail funName, funDeps))
traverseOverUValBind expr = Nothing

getFunctionNameFromValBind :: Ann UMatchLhs (Dom GhcPs) SrcTemplateStage -> String
getFunctionNameFromValBind expr@(Ann _ (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) ex1)) = ex
    -- print ex1
    -- -- print ex
    -- (pure ex)

getFunctionsCalledInFunction :: Ann URhs (Dom GhcPs) SrcTemplateStage -> [String]
getFunctionsCalledInFunction expr = do
    let !funs = filter (/= "") $ map (getFunctions') (expr ^? biplateRef)
    funs

getFunctions' :: Ann UName (Dom GhcPs) SrcTemplateStage -> String
getFunctions' expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = ex
getFunctions' expr = ""



-- logicFn expr@(Let (LocalBinds [(SimpleBind _ (UnguardedRhs (Paren (App (Var arg1) (Var arg2)))) _ )]) (Var arg)) = do
--     let !y1 = trace ("Extra getFiled 1" ++ (showSDocUnsafe $ ppr $ idType $ semanticsId arg)) $ showName arg
--     let !y2 = trace ("Extra getFiled 1" ++ (showSDocUnsafe $ ppr $ idType $ semanticsId arg1)) $ showName arg
--     return expr

-- ULet {_exprFunBind = [ULocalValBind {_localVal = USimpleBind {_valBindPat = UVarPat {_patternName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "dongg"}}}},
--                                                               _valBindRhs = UUnguardedRhs {_rhsExpr = UApp {_exprFun = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "encodeDecodeTransform"}}}},
--                                                                                                             _exprArg = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "keyUuid"}}}}}},
--                                                               _valBindLocals = Nothing}}], _exprInner = UVar {_exprName = UNormalName {_simpleName = UQualifiedName {_qualifiers = [], _unqualifiedName = UNamePart {_simpleNameStr = "dongg"}}}}}

-- logicFn expr@(Let (AST.AnnListG annot [LocalValBind (SimpleBind _ (UnguardedRhs ((App (Var arg1) (Var arg2)))) _ )]) (Var arg)) = do
--     let !y1 = trace ("Extra LocalValBind 1 " ++ (showSDocUnsafe $ ppr $ idType $ semanticsId arg)) $ showName arg
--     let !y2 = trace ("Extra LocalValBind 1 " ++ (showSDocUnsafe $ ppr $ idType $ semanticsId arg2)) $ showName arg
--     return $ mkVar $ mkName $ "Just " ++ showName arg2
-- logicFn expr@(Let (AST.AnnListG annot [LocalValBind (SimpleBind _ (UnguardedRhs (Paren (App (Var arg1) (Var arg2)))) _ )]) (Var arg)) = do
--     -- let y = x ^. simpleBind
--     let !y1 = trace ("Extra LocalValBind 1 " ++ (showSDocUnsafe $ ppr $ idType $ semanticsId arg)) $ showName arg
--     let !y2 = trace ("Extra LocalValBind 1 " ++ (showSDocUnsafe $ ppr $ idType $ semanticsId arg2)) $ showName arg
--     return expr
-- logicFn expr@(InfixApp (Var arg) op (Var arg1)) = do
--     let !y1 = trace ("Extra getFiled 1" ++ (showSDocUnsafe $ ppr $ idType $ semanticsId arg)) $ showName arg
--     let !y2 = trace ("Extra getFiled 1" ++ (showSDocUnsafe $ ppr $ idType $ semanticsId arg1)) $ showName arg
--     return expr