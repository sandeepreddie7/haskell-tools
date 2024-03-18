{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Haskell.Tools.Parser.FlowChange where

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
import Data.List.Extra (replace, stripSuffix)
import Shelly
import Language.Haskell.Tools.Parser.RemoveUnusedFuns (getFunctionDepOfModule')
import qualified Language.Haskell.Tools.Parser.GetFunctionBranching as GFT
import Control.Applicative ((<|>))
import Language.Haskell.Tools.Refactor as HT
import qualified Debug.Trace as Trace
import qualified Data.Text as DT
import Control.Exception

import Language.Haskell.Tools.AST.Representation.Binds (ULocalBind)
import qualified Language.Haskell.Tools.Parser.RemoveUnusedFuns as RS

data FunctionModified =
  FunctionModified  {
      deleted :: [String]
    , modified :: [String]
    , added :: [String]
    , modName :: String
    }
  deriving (Show)

-- listDir :: IO String
-- listDir commit1 commit2 = do
--   shelly $ do
--     "git diff commit1 commit2"

-- defaultFunctionModified =
--     FunctionModified "defaultFile" [] [] []

-- NOTE: Ideally we should add same module functions in FunctionModified
addFunctionModifed funMod funMod2 = FunctionModified (deleted funMod ++ deleted funMod2) (modified funMod ++ modified funMod2) (added funMod ++ added funMod2) (modName funMod)
 
compareFile1 = "Euler.API.Gateway.Gateway.Adyen.Transforms.Refund"
compareFile2 = "Euler.API.Gateway.Gateway.Adyen.Transforms.ModRefund"

compareASTForFuns :: String -> String -> IO ()
compareASTForFuns filePath moduleName' = do
    moduleASTOld <- moduleParser filePath moduleName'
    moduleASTNew <- moduleParser filePath moduleName'
    let oldFuns = HM.fromList $ getAllFunctions moduleASTOld
    let newFuns = HM.fromList $ getAllFunctions moduleASTNew
    let removed = HM.keys $ HM.difference newFuns oldFuns
    -- print removed
    let !y = HM.foldlWithKey (\acc k val ->
                case HM.lookup k newFuns of
                Just newVal -> if ((show val) == (show newVal)) then acc else addFunctionModifed acc (FunctionModified [] [k] [] moduleName')
                Nothing -> addFunctionModifed acc (FunctionModified [k] [] [] moduleName') ) (FunctionModified [] [] removed moduleName') oldFuns
    print y

traverseOverUValBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, Ann UDecl (Dom GhcPs) SrcTemplateStage)
traverseOverUValBind expr@(Ann _ (UValueBinding (FunctionBind' ex))) = do
    let funName = mapMaybe (getFunctionNameFromValBind) ((ex) ^? biplateRef)
    if not $ null funName then do
        Just (head funName, expr)
    else Nothing
traverseOverUValBind expr@(Ann _ (UValueBinding (Ann _ (USimpleBind pat _ ex)))) = do
    let funName = fromJust $ getPatternName' pat
    if not $ null funName then do
        Just (head funName, expr)
    else Nothing
traverseOverUValBind expr = Nothing

traverseOverClassBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> [(String, Ann UDecl (Dom GhcPs) SrcTemplateStage)]
traverseOverClassBind expr@(Ann _ (UClassDecl _ _ _ (AnnMaybeG _ (Just (Ann _ (UClassBody (AnnListG _ body))))))) =
    mapMaybe (\x -> do
                val <- getClassFun x
                pure (val,expr)) $ body
traverseOverClassBind expr = []

getClassFun :: Ann UClassElement (Dom GhcPs) SrcTemplateStage -> Maybe (String)
getClassFun expr@(Ann _ (UClsSig (Ann _ (UTypeSignature funName _)))) = Just (head $ map getFunctions' $ funName ^? biplateRef)
getClassFun expr = Nothing

getFunctionNameFromValBind :: Ann UMatch (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionNameFromValBind expr@(Ann _ (UMatch (Ann _ (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) ex1)) _ _)) = Just ex
getFunctionNameFromValBind _ = Nothing

getAllFunctions moduleAST =
    let classBinds = concat $ map (\x -> traverseOverClassBind x) (moduleAST ^? biplateRef)
        funBinds = mapMaybe (\x -> traverseOverUValBind x) (moduleAST ^? biplateRef)
    in funBinds ++ classBinds

output = ["/home/chaitanya/Desktop/work/euler-api-gateway/src/Euler/API/Gateway/Gateway/PineLabs/Flows/Emi.hs", "/home/chaitanya/Desktop/work/euler-api-gateway/src/Euler/API/Gateway/Gateway/PineLabs/Flows.hs","/home/chaitanya/Desktop/work/euler-api-gateway/src/Euler/API/Gateway/Gateway/Common.hs","/home/chaitanya/Desktop/work/euler-api-gateway/src/Euler/API/Gateway/App/Routes.hs","/home/chaitanya/Desktop/work/euler-api-gateway/src/Euler/API/Gateway/App/Server.hs"]

getParentFun :: String -> IO [String]
getParentFun modName = do
    let allFiles =  filter (/="Main.hs") $ RS.getAllSubFils "/home/chaitanya/Desktop/work/euler-api-gateway/src" []
    let importsHashMap = (HM.empty :: HM.HashMap String [String])
    getImpsHm <- mapM (\x -> do
                         imps <- parseImportsFromFile x
                         pure (x,imps)) allFiles
    val <- getAllDepFiles allFiles (HM.fromList getImpsHm) "Euler.API.Gateway.Gateway.PineLabs.Flows.Emi"
    pure val
  where
    -- getAllDepFiles [] _ _ = pure []
    getAllDepFiles allFiles importsHashMap modCheck = do
        x <- filterM (\fileName -> parseAllFiles modCheck fileName importsHashMap) allFiles
        y <- mapM (\t -> getAllDepFiles allFiles importsHashMap (fromMaybe "" $ stripSuffix ".hs" $ replace "/" "." $ fromMaybe "" $ stripPrefix "/home/chaitanya/Desktop/work/euler-api-gateway/src/" t)) x
        pure $ nub $ x ++ (concat y)

        -- pure ()
    -- moduleAST <- mapM (\x -> moduleParser "/home/chaitanya/Desktop/work/euler-api-gateway/src" modName
    -- let importHM = foldl' (\acc x -> parseImportsAndGetModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])

getParentFunctions :: IO [String]
getParentFunctions = do
    let fun = "normalizeBankCode"
    allFunList <- foldM (\acc t -> do
        let name = fromMaybe "" $ stripSuffix ".hs" $ replace "/" "." $ fromMaybe "" $ stripPrefix "/home/chaitanya/Desktop/work/euler-api-gateway/src/" t
        x <- undefined -- getModFunctionList name
        pure x) [] output
    -- filteredFuns <-
    --          (foldM (\acc (funName,y) -> do
    --                 if funName == "transformEmisForBank" then print (snd <$> y) else pure ()
    --                 pure $ if fun `elem` (snd <$> y) then acc ++ [funName] else acc)) [] x
    pure []
    where
        getDepFuns allFunList funName = undefined
            -- filteredFuns <-
            --  (foldM (\acc (funName,y) -> do
            --         if funName == "transformEmisForBank" then print (snd <$> y) else pure ()
            --         pure $ if fun `elem` (snd <$> y) then acc ++ [funName] else acc)) [] x
            -- pure filteredFuns

parseAllFiles :: String -> String -> _ -> IO Bool
parseAllFiles modCheck modName importsHashMap = do
    (importsList) <-
      case HM.lookup modName importsHashMap of
        Just val -> do
            print ("Got file " ++ modName)
            pure (val)
        Nothing -> do
            print ("Parsing file " ++ modName ++ modCheck)
            content <- readFile modName
            impList <- mapM (\x -> if "import " `Data.List.isPrefixOf` x then getImportModuleName x else pure "") (Data.List.lines content)
            pure (impList)
    pure $ (if modCheck `elem` importsList then True else False)

parseImportsFromFile modName = do
    content <- readFile modName
    impList <- mapM (\x -> if "import " `Data.List.isPrefixOf` x then getImportModuleName x else pure "") (Data.List.lines content)
    pure (impList)

parseImportsAndGetModule :: _ -> String
parseImportsAndGetModule x@(GHC.ImportDecl _ _ modName pkgQual isBoot _ isQual _ asMod explicitImports) = (GHC.moduleNameString $ unLoc modName)

getModFunctionList :: _ -> String -> IO [(String, [(String, String)])]
getModFunctionList moduleAST modName = do
    let allFuns = getAllFunctions moduleAST
    !originalList <- mapM (\(k,val) -> do
                    funDeps <- getAllFunctionDeps val k
                    pure $ (k, funDeps)) allFuns

    updateMods <- mapM (\(k,v) -> do
                      val <- mapWithModName v (HM.fromList allFuns) moduleAST modName
                      pure (k,(concat val))) (originalList)
    -- print importHMQ
    pure updateMods
    where
        mapWithModName funList allFuns moduleAST modName = do
            let importHM = foldl' (\acc x -> GFT.parseImportsAndGetModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
            let importHMQ = foldl' (\acc x -> GFT.parseImportsAndGetQualModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
            -- importHMQAll <- foldM (\acc x -> parseModsAndGetFuns x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
            -- print (importHM,importHMQ)
            pure $ map (\(fun@(x,y)) -> case (snd <$> HM.lookup (y) importHM) <|> (HM.lookup (fromMaybe "" x) importHMQ)of
                                    Just imp -> (imp,y)
                                    Nothing -> ("",y)) <$> funList

parseModsAndGetFuns :: Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> HM.HashMap String [String] -> IO (HM.HashMap String [String])
parseModsAndGetFuns expr@(Ann _ (UImportDecl _ _ _ _ moduleName qualifiedName specs)) hm = do
   case GFT.getModuleName moduleName of
    Just modName -> do
        emodAst <- try $ moduleParser "/home/chaitanya/Desktop/work/euler-api-gateway/src" (HT.trace ("Iam here" ++ show modName) modName)
        case emodAst of
            Right modAst -> do
                let allFuns = mapMaybe (\x -> traverseOverSigBind x ) (modAst ^? biplateRef)
                pure $ HM.insert modName allFuns hm
            Left (e :: SomeException) -> pure hm
    Nothing -> HT.trace ("Iam here" ++ show expr) $ pure hm

traverseOverSigBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe String
traverseOverSigBind expr@(Ann _ (UTypeSigDecl (Ann _ (UTypeSignature (AnnListG _ names) _)))) = getNamePart $ head names
traverseOverSigBind _ = Nothing

getAllFunctionDeps :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> String -> IO [[(Maybe String, String)]]
getAllFunctionDeps moduleAST funName = do
    let funDepsList =  mapMaybe (\x -> GFT.traverseOverUValBind x funName ) (moduleAST ^? biplateRef)
    let originalList = concat $ map (GFT.getRhsName') (funDepsList ^? biplateRef)
    pure originalList

getFunctionNameFromSimpleBind :: String -> Ann UValueBind (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionNameFromSimpleBind str expr@(Ann _ (USimpleBind pat _ _)) = 
    let name = fromJust $ getPatternName' pat
    in if null name then Nothing else Just $ headMaybe ("getFunctionNameFromSimpleBind " <> str) name
getFunctionNameFromSimpleBind str expr@(Ann _ (UFunBind ex)) =
    let name = mapMaybe getFunctionNameFromValBind (ex ^? biplateRef)
    in if null name then Nothing else Just $ headMaybe ("getFunctionNameFromSimpleBind " <> str) name

headMaybe :: String -> [a] -> a
headMaybe err list = if null list then error $ "HEAD ERROR " <> err else head list

getPatternName' :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getPatternName' expr = Just $ mapMaybe getNamePart (expr ^? biplateRef)

getNamePart :: Ann UName (Dom GhcPs) SrcTemplateStage -> Maybe String
getNamePart expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = Just ex
getNamePart expr = Nothing

getFunctionsCalledInFunction :: Ann URhs (Dom GhcPs) SrcTemplateStage -> [String]
getFunctionsCalledInFunction expr = do
    let !funs = filter (/= "") $ map (getFunctions') (expr ^? biplateRef)
    nub funs

getFunctions' :: Ann UName (Dom GhcPs) SrcTemplateStage -> String
getFunctions' expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = ex
getFunctions' expr = ""

getImportModuleName :: String -> IO ((String))
getImportModuleName importLine = do
    val <- try $ runGhc (Just libdir) $ parseImportDecl importLine
    case val of
        Left (e :: SomeException) -> pure ""
        Right val -> pure $ parseImportsAndGetModule val