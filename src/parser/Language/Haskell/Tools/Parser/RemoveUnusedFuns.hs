
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}

module Language.Haskell.Tools.Parser.RemoveUnusedFuns where

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
import Data.Char (isAlphaNum)
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
import Language.Haskell.Tools.Parser.ParseModule
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

import Language.Haskell.Tools.AST.Representation.Binds (ULocalBind)





-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

parseAndGetFuns = do
    contents <- readFile "/home/chaitanya/Downloads/euler-api-txns/unwantedGrepp.txt"
    let arr = map (\x -> splitOn ":" x) (lines contents)
    let val = 
         foldl' (\acc x ->
           acc ++ [((head x),fromMaybe "" $ stripPrefix  " " $ last x)]) ([] :: [(String,String)]) arr
    mapM (\x -> do
        let path = head $ splitOn "/" $ fst $ head x
            funs = snd <$> x
            modName = fromMaybe "" $ stripSuffix ".hs" $ intercalate "." $ tail $ splitOn "/" $ fst $ head x
        let srcPath = projectPath ++ getSrcPath (fst $ head x) ++ path ++ "/"
        pure $ (srcPath,modName, funs)) $  groupBy (\a b -> fst a == fst b ) val

getSrcPath path
   | isEulerxPath == True = "euler-x/"
   | deciderPath == True = "euler-api-decider/"
   | oltpPath == True = "oltp/"
   | otherwise = ""
  where 
    isEulerxPath = unsafePerformIO $ doesFileExist $ projectPath ++ "euler-x/" ++ path
    deciderPath = unsafePerformIO $ doesFileExist $ projectPath ++ "euler-api-decider/" ++ path
    oltpPath = unsafePerformIO $ doesFileExist $ projectPath ++ "oltp/" ++ path
    
        
projectPath = "/home/chaitanya/Downloads/euler-api-txns/"

removeUnwantedFuns = do
    unwantedFuns <- parseAndGetFuns
    mapM (\(x,y,z) -> removeUnwantedFunsFromFile x y z 
         ) unwantedFuns

removeUnwantedFunsFromFile :: String -> String -> [String] -> IO _
removeUnwantedFunsFromFile modulePath moduleName funs = do
    print modulePath
    print moduleName
    moduleAST <- moduleParser modulePath moduleName
    (AST.AnnListG annot currentDecl) <- moduleAST ^? (modDecl) 
    let decls = filter (\decl -> not $ checkFun decl funs) currentDecl
        nAST = (.=) modDecl (AST.AnnListG annot decls) moduleAST
    writeFile (modulePath <> (replace "." "/" moduleName) <> ".hs") (prettyPrint nAST)

getAllSubFils :: String -> [String] -> [String]
getAllSubFils dir files = do
   nub $ filter (\x -> ".hs" `isSuffixOf` x ) $ if unsafePerformIO $ (doesDirectoryExist dir) then do
      let allFiles = unsafePerformIO $ listDirectory (dir)
      Data.List.Extra.foldl (\acc x -> getAllSubFils (dir <> "/" <> x) acc) files allFiles
    else files ++ [dir]


getFunctionDeps :: String -> String -> IO (HM.HashMap String [String])
getFunctionDeps modulePath moduleName = do
    moduleAST <- moduleParser modulePath moduleName
    getFunctionDepOfModule moduleAST

getFunctionDepOfModule :: (Ann AST.UModule (Dom GhcPs) SrcTemplateStage) -> IO (HM.HashMap String [String])
getFunctionDepOfModule moduleAST = do
    let !funDepList = mapMaybe (traverseOverUValBind) (moduleAST ^? biplateRef)
        funList = HM.fromList $ map (\(fun,y) -> (fun, (fst y , nub $ filter (\x -> x `elem` (fst <$> funDepList)) (snd y)))) funDepList
    pure $ HM.foldlWithKey' (\acc key (x,y) -> HM.insert key (nub $ getAllRelatedFuns x y funList) acc) HM.empty funList
    where
        getAllRelatedFuns whereDeps funs funList = foldl' (\acc val -> case HM.lookup val funList of
                                                                    Just f -> acc ++ snd f
                                                                    Nothing -> acc) funs whereDeps

getFunctionDepOfModule' :: (Ann AST.UModule (Dom GhcPs) SrcTemplateStage) -> IO (HM.HashMap String [String])
getFunctionDepOfModule' moduleAST = do
    let !funDepList = mapMaybe (traverseOverUValBind) (moduleAST ^? biplateRef)
        funList = HM.fromList $ map (\(fun,y) -> (fun, (fst y , nub $ filter (\x -> x `elem` (fst <$> funDepList)) (snd y)))) funDepList
        whereClauseList = HM.foldlWithKey' (\acc key (x,y) -> HM.insert key (x) acc) HM.empty funList
    print funDepList
    print ("whereDeps" ++ (show $ getFunsOfWhereClause $ HM.fromList funDepList))
    pure $ HM.foldlWithKey' (\acc key (x,y) -> HM.insert key (nub $ getAllRelatedFuns x y funList) acc) HM.empty funList 
    where
        getAllRelatedFuns whereDeps funs funList = foldl' (\acc val -> case HM.lookup val funList of
                                                                    Just f -> acc ++ snd f
                                                                    Nothing -> acc) funs whereDeps

getFunsOfWhereClause :: HM.HashMap String ([String],[String]) -> HM.HashMap String [String]
getFunsOfWhereClause hm = HM.foldl' (\acc (x,_) -> getAllFunctions acc x hm) HM.empty hm

getAllFunctions :: HM.HashMap String [String] -> [String] -> HM.HashMap String ([String],[String]) -> HM.HashMap String [String]
getAllFunctions whereHm listOfWhere funList =
    foldl' (\acc x -> 
                case HM.lookup x funList of
                    Just val -> 
                        -- let allFuns = getAllFunctions acc (fst val) funList
                        HM.insert x (fst val) acc
                    Nothing -> acc
    ) whereHm listOfWhere

suffixToBeAdded :: String
suffixToBeAdded = "Split"

moduleSuffix :: String -> String
moduleSuffix modulePath = replace modulePath ".hs" ("/" ++ suffixToBeAdded) -- moduleName to be added

splitAndWrite :: String -> String -> String -> String -> IO Int
splitAndWrite modulePath moduleName groupedFunctionsString modFunReferenceString = do
    moduleAST <- moduleParser modulePath moduleName
    resolveDeps <- getFunctionDepOfModule moduleAST
    let groupedFunctions = fromMaybe (mempty) (A.decode $ A.encode groupedFunctionsString)  
    let modFunReference = fromMaybe (mempty) (A.decode $ A.encode modFunReferenceString)  
    writeBackGroupedModules modulePath moduleName resolveDeps moduleAST groupedFunctions modFunReference

writeBackGroupedModules :: String -> String -> HM.HashMap String [String] -> (Ann AST.UModule (Dom GhcPs) SrcTemplateStage) -> [[String]] -> HM.HashMap String Int -> IO Int
writeBackGroupedModules modulePath moduleName resolveDeps moduleAST groupedFunctions modFunReference = do
    -- fun list generated by python
    (AST.AnnListG annot currentDecl) <- moduleAST ^? (modDecl) 
    let newImportsList = map nub $ snd $ foldl' (\(l,acc) fun -> (l+1,acc ++ [foldl' (\(acc) x -> maybe (acc) (\val -> ((fetchNum val l) ++ acc)) $ HM.lookup x resolveDeps) ([]) fun])) (0,[]) groupedFunctions
    !newModDecl <- mapM (\(x,y) -> do
                            let decls = filter (\decl -> checkFun decl x) currentDecl
                                nAST = (.=) modDecl (AST.AnnListG annot decls) moduleAST
                            impsFil <- ((\(AST.AnnListG annot val) -> pure $ modifyImports y val) =<< (nAST ^? modImports)) --(.=) modDecl (AST.AnnListG annot decls) nAST
                            pure $ (.=) modImports (AST.AnnListG annot impsFil) nAST
                        ) (zip groupedFunctions newImportsList) 
    let newModDeclForRoot = let decls = filter (\decl -> not $ checkFun decl (concat groupedFunctions)) currentDecl
                                in (.=) modDecl (AST.AnnListG annot decls) moduleAST
    writeFile modulePath  (prettyPrint newModDeclForRoot)
    foldM (\acc x -> do
        changedModName <- liftIO $ (!~) (biplateRef) (changeModName (("." ++ suffixToBeAdded) ++ (show acc))) (x)
        (writeFile ((moduleSuffix modulePath) ++ (show acc) ++ ".hs") (prettyPrint changedModName))
        pure $ (acc + 1)) 0 newModDecl

    where
        modifyImports :: [String] -> [Ann UImportDecl (Dom GhcPs) SrcTemplateStage] -> [Ann UImportDecl (Dom GhcPs) SrcTemplateStage]
        modifyImports imps val =  foldl' (\(acc) x ->  ( acc ++ [mkImportDecl' False False False Nothing (mkModuleName' x) Nothing Nothing])) (val) imps
        fetchNum :: [String] -> Int -> [String]
        fetchNum val l = foldl' (\(acc) x -> maybe (acc) (\y -> if l == y then acc else ([moduleName ++ suffixToBeAdded ++ (show y)] ++ acc)) $ HM.lookup x modFunReference) [] val


changeModName :: String -> Ann UModuleHead (Dom GhcPs) SrcTemplateStage -> IO (Ann UModuleHead (Dom GhcPs) SrcTemplateStage)
changeModName str expr@(Ann y ((UModuleHead (Ann b (UModuleName name)) prag (AnnMaybeG x _)))) = do
   let names = name ++ str
   print names
   let modName = mkModuleName' names
   let modHead = mkModuleHead' modName Nothing Nothing
   pure modHead


getNamePart :: Ann UName (Dom GhcPs) SrcTemplateStage -> Maybe String
getNamePart expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = Just ex
getNamePart expr = Nothing

getPatternName' :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getPatternName' expr = Just $ mapMaybe getNamePart (expr ^? biplateRef)

checkFun :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> [String] -> Bool
checkFun expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature funName _)))) str = any (== (head $ map getFunctions' $ funName ^? biplateRef)) str
checkFun expr@(Ann _ (UValueBinding (FunctionBind' ex))) str =
    let !funName = mapMaybe (getFunctionNameFromValBind) ((ex) ^? biplateRef)
    in if null funName then False else any (== head funName) str
checkFun expr@(Ann _ (UValueBinding (Ann _ (USimpleBind pat _ _)))) str = 
        let name = fromJust $ getPatternName' pat
        in if null name then False else any (== head name) str
checkFun expr _ = False

traverseOverUValBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, ([String],[String]))
traverseOverUValBind expr@(Ann _ (UValueBinding (FunctionBind' ex))) = do
    let !funName = mapMaybe (getFunctionNameFromValBind) ((ex) ^? biplateRef)
    if not $ null funName then do
        let !funNameMap = (head funName, tail funName)
            !funDeps = concat $ map (getFunctionsCalledInFunction) (expr ^? biplateRef)
        Just (head funName, (tail funName, funDeps))
    else Nothing
traverseOverUValBind expr = Nothing

getFunctionNameFromValBind :: Ann UMatchLhs (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionNameFromValBind expr@(Ann _ (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) ex1)) = Just ex
getFunctionNameFromValBind _ = Nothing

getFunctionsCalledInFunction :: Ann URhs (Dom GhcPs) SrcTemplateStage -> [String]
getFunctionsCalledInFunction expr = do
    let !funs = filter (/= "") $ map (getFunctions') (expr ^? biplateRef)
    funs

getFunctions' :: Ann UName (Dom GhcPs) SrcTemplateStage -> String
getFunctions' expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = ex
getFunctions' expr = ""

