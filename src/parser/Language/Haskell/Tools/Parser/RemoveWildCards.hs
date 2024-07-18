{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Parser.RemoveWildCards where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint
import Data.List
import GHC
import Control.Reference
import Language.Haskell.Tools.Parser.ParseModule
import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances
import Control.Monad.Extra
import qualified Data.HashMap.Strict as HM
import Data.Char (toLower)
import Data.List.Extra (replace)
import Language.Haskell.Tools.Parser.RemoveUnusedFuns hiding (traverseOverUValBind)

removeWildCards :: String -> String -> IO ()
removeWildCards modulePath moduleName = do
    moduleAST <- moduleParser modulePath moduleName
    moduleASTMod <- (!~) (biplateRef) (changeAsPat) (moduleAST)
    newAST <- (!~) (biplateRef) (traverseOverUValBind) (moduleASTMod)
    writeFile (modulePath <> (replace "." "/" moduleName) <> ".hs") (prettyPrint newAST)

changeAsPat :: Ann UMatchLhs (Dom GhcPs) SrcTemplateStage -> IO (Ann UMatchLhs (Dom GhcPs) SrcTemplateStage)
changeAsPat expr@(Ann _ (UNormalLhs name (AnnListG _ args))) = pure $
   let modifiedArgs = if length args == 1
                        then foldl' (\acc x -> acc ++ [addAsPat x Nothing]) [] args
                        else snd $ foldl' (\(c,acc) x -> (c+1,acc ++ [addAsPat x (Just c)])) (0,[]) args
   in
    mkMatchLhs' name $ modifiedArgs
  where
    addAsPat :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> Maybe Int -> Ann UPattern (Dom GhcPs) SrcTemplateStage
    addAsPat expr@(Ann _ (UParenPat (Ann _ (URecPat patternName _)))) count =
        let (firstLetter, newName) = fromMaybe ('a', []) $ uncons $ getFieldName patternName
        in mkAsPat' (mkName' ([toLower firstLetter] ++ newName ++ (fromMaybe "" $ show <$> count))) expr
    addAsPat expr@(Ann _ (URecPat patternName _)) count =
        let (firstLetter, newName) = fromMaybe ('a', []) $ uncons $ getFieldName patternName
        in mkAsPat' (mkName' ([toLower firstLetter] ++ newName ++ (fromMaybe "" $ show <$>  count))) expr
    addAsPat expr _ = expr
changeAsPat expr = pure expr

traverseOverUValBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UDecl (Dom GhcPs) SrcTemplateStage)
traverseOverUValBind expr@(Ann _ (UValueBinding (FunctionBind' ex))) = do
    !wildCardList <- mapMaybeM (wildCardsRefactor) (ex ^? biplateRef)
    let newList = lookUpAndConcat (HM.unions wildCardList)
    print newList
    newAST <- (!~) (biplateRef) (removeWildPats) (expr)
    newAST' <- (!~) (biplateRef) (changeWildCards newList) (newAST)
    -- pure expr
    pure newAST'
  where
    lookUpAndConcat :: (HM.HashMap String (String, String)) -> (HM.HashMap String String)
    lookUpAndConcat hashMap = HM.foldlWithKey (\acc k v -> HM.insert k (fromMaybe "" $ getValue k v hashMap) acc) HM.empty hashMap
    getValue k v hashMap =
        case HM.lookup k hashMap of
          Nothing -> Nothing
          Just (a,b) -> Just $ maybe (a ++ "." ++ b) (\x -> x ++ "." ++ b) $ getValue a v hashMap
traverseOverUValBind expr = pure expr

getFieldName :: Ann UName (Dom GhcPs) SrcTemplateStage -> String
getFieldName expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = ex
getFieldName expr = ""

changeWildCards :: HM.HashMap String (String) -> Ann UName (Dom GhcPs) SrcTemplateStage -> IO (Ann UName (Dom GhcPs) SrcTemplateStage)
changeWildCards list expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = 
  pure
   $ case HM.lookup ex list of
       Nothing -> expr
       Just val -> mkName' val
changeWildCards _ expr = pure expr

getNameFromPattern :: Ann UPatternField (Dom GhcPs) SrcTemplateStage -> (String, String)
getNameFromPattern expr@(Ann _ (UNormalFieldPattern fieldName (Ann _ (UVarPat (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))))))) = (getFieldName fieldName, ex)
getNameFromPattern expr@(Ann _ (UNormalFieldPattern fieldName ex@(Ann _ (UAsPat patternName _)))) = (getFieldName fieldName,getFieldName patternName)
getNameFromPattern expr = ("", "")

-- Get all let statements, Group and Refactor
wildCardsRefactor :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> IO (Maybe (HM.HashMap String (String,String)))
wildCardsRefactor expr@(Ann _ (UAsPat patternName patternInner)) = do
    let name = getFieldName patternName
    allFileds <- getAllFields patternInner
    pure $ Just $ foldl' (\acc (x,y) -> HM.insert y (name,x) acc) HM.empty allFileds
wildCardsRefactor _ = pure Nothing

removeWildPats :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> IO (Ann UPattern (Dom GhcPs) SrcTemplateStage)
removeWildPats expr@(Ann _ (UAsPat patternName patternInner@(Ann _ (UParenPat (Ann _ (URecPat _ _)))))) =
    let pat = mkVarPat' patternName
    in pure pat
removeWildPats expr@(Ann _ (UAsPat patternName patternInner@(Ann _ (URecPat _ _)))) =
    let pat = mkVarPat' patternName
    in pure pat
removeWildPats expr = pure expr

getAllFields :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> IO [(String,String)]
getAllFields expr@(Ann _ (UParenPat (Ann _ (URecPat patternName (AnnListG _ patternFields))))) =
    let name = map getNameFromPattern patternFields
    in pure name
getAllFields expr@(Ann _ (URecPat patternName (AnnListG _ patternFields))) =
    let name = map getNameFromPattern patternFields
    in pure name
getAllFields expr = pure []

-- Modify if let statement
modifiyStmts :: [Ann UStmt (Dom GhcPs) SrcTemplateStage] -> [Ann UStmt (Dom GhcPs) SrcTemplateStage]
modifiyStmts expr =
    if (checkIfLet $ head expr)
        then modifyLets expr
        else expr

checkIfLet :: Ann UStmt (Dom GhcPs) SrcTemplateStage -> Bool
checkIfLet (Ann _ (ULetStmt _)) = True
checkIfLet _ = False

-- Let modifications
modifyLets :: [Ann UStmt (Dom GhcPs) SrcTemplateStage] -> [Ann UStmt (Dom GhcPs) SrcTemplateStage]
modifyLets expr =
    let (Ann _ (ULetStmt (AnnListG _ ex))) = head expr
        otherExpr = map getLocalBinding $ tail expr
    in [mkLetStmt' $ (ex ++ (concat otherExpr))]
  where
    getLocalBinding (Ann _ (ULetStmt (AnnListG _ expr))) = expr

findAndReplace :: HM.HashMap String [String] -> String -> String -> IO ()
findAndReplace hm oldFilePath newFilePath = do
    y <- mapM (\(key,value) -> parseAndGet key value oldFilePath newFilePath) (HM.toList hm)
    print y

parseAndGet :: String -> [String] -> String -> String -> IO String
parseAndGet moduleName listOfFuns oldFilePath newFilePath = do
    newAST <- moduleParser newFilePath moduleName
    (AnnListG _ newDecls) <- newAST ^? modDecl
    oldAST <- moduleParser oldFilePath moduleName
    (AnnListG annot oldDecls) <- oldAST ^? modDecl
    let allDecls = filter (\decl -> not $ checkDecl decl listOfFuns) oldDecls
        newAddedDecls = allDecls ++ newDecls
        modifiedAST = (.=) modDecl (AnnListG annot newAddedDecls) oldAST
    writeFile (oldFilePath <> (replace "." "/" moduleName) <> ".hs" ) (prettyPrint modifiedAST)
    pure ""
    


checkDecl :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> [String] -> Bool
checkDecl expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature funName _)))) str = any (== (head $ map getFunctions' $ funName ^? biplateRef)) str
checkDecl expr@(Ann _ (UValueBinding (FunctionBind' ex))) str =
    let !funName = mapMaybe (getFunctionNameFromValBind) ((ex) ^? biplateRef)
    in if null funName then False else any (== head funName) str
checkDecl expr@(Ann _ (UValueBinding (Ann _ (USimpleBind pat _ _)))) str = 
        let name = fromJust $ getPatternName' pat
        in if null name then False else any (== head name) str
checkDecl expr _ = False


