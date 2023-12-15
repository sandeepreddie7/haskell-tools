{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Haskell.Tools.Parser.GetFunctionBranching where

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
import Data.Char (toLower, isUpper)
import Data.List.Extra (replace)
import Language.Haskell.Tools.Parser.SplitModule (getFunctionDepOfModule')
import Language.Haskell.Tools.Refactor as HT
import Control.Applicative ((<|>))
import System.Directory (doesPathExist)
import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.Tree
import Text.Pretty.Simple

getPrePendPath = "/home/chaitanya/Desktop/work/euler-api-txns/euler-x/"

data FlowTree = FlowNode String [FlowTree]
    deriving Show

instance Semigroup FlowTree where
    (FlowNode name1 children1) <> (FlowNode name2 children2) =
        if name1 == "" || name2 == "" then
          FlowNode (name1 ++ name2) (children1 <> children2)
        else
          FlowNode (name1 ++ "__" ++ name2) (children1 <> children2)
    
instance Monoid FlowTree where
    mempty = FlowNode "" []

isLocal :: String -> (Maybe String,Bool)
isLocal moduleName =
   let tryAllPossiblePathsWithSplit = (\d -> getPrePendPath <> d <> "/" <> replace "." "/" moduleName <> ".hs") <$> ["src", "src-generated", "src-extras"]
       filePaths = filter isFileLocal (tryAllPossiblePathsWithSplit)
       path = fromMaybe "" $ if null filePaths then Nothing else Just $ head filePaths
       orgPath = if isInfixOf "src-generated" path then getPrePendPath <> "src-generated/" else if isInfixOf "src-extras" path then  getPrePendPath <> "src-extras/" else getPrePendPath <> "src/"
    in (if orgPath == "" then Nothing else Just orgPath , (moduleName `elem` ["ECPrelude", "NOT_FOUND", "Utils.Monitoring", "Config.Constants", "PS.Data.String", "ShimPrelude", "Config", "Nau.InlineJS", "Nau.InlineJS.Session", "Utils.Log.Shims", "Utils.Log", "Types.Lenses"]) || (null filePaths ) || isPrefixOf "PS.Data." moduleName)
   where
    isFileLocal :: String -> Bool
    isFileLocal lPath = unsafePerformIO $ doesPathExist lPath

funListIgnore = []

getFunctionTree :: String -> String -> IO _
getFunctionTree modulePath moduleName = do
    funMapsHM <- newMVar HM.empty
    modMapsHM <- newMVar HM.empty
    redList <- newMVar []
    val <- getAllFunction modulePath moduleName "funName" (redList, funMapsHM) modMapsHM
    writeFile "fungraphfunName" (drawTree $ toDataTree $ FlowNode "funName" [fromJust val])
    writeFile "fungraphTreefunName" (show $ toDataTree $ FlowNode "funName" [fromJust val])
    pure $ toDataTree $ FlowNode "funName" [fromJust val]

-- generateFlowTree :: String -> String -> String -> MVar (HM.HashMap String _) -> (MVar [String],MVar (HM.HashMap String ([[(String,String)]]))) -> IO FlowTree
-- generateFlowTree modulePath moduleName functionName mVarModHm (mVarRedList,mvarHm) =
--     allFunList <- getFunctions modulePath moduleName functionName mVarModHm
--     flowTree <- (sequence $ mapM (\x -> generateFlowTree modulePath (fst x) (snd x) mVarModHm (mVarRedList,mvarHm)) <$> (allFunList))
--     if isStorageCall functionName
--     then do
--        pure $ FlowNode functionName flowTree
--     else if


--         pure $ FlowNode "" []

isStorageCall :: String -> Bool
isStorageCall str = True 

getAllFunction :: String -> String -> String -> (MVar [String], MVar (HM.HashMap String ([[(String,String)]]))) -> MVar (HM.HashMap String _) -> IO (Maybe FlowTree)
getAllFunction modulePath moduleName funGraph (mVarRedList,mvarHm) mVarModHm = do 
    allFunList <- getFunAndHM
    print allFunList
    redList <- readMVar mVarRedList
    let reductantList = nub redList
    print funGraph
    foldM (\a y -> do
      val <- foldM (\acc x -> do
        if isStorageCall (snd x)
            then pure $ Just (FlowNode (snd x) [])  -- acc ++ [x]
            else
              if (snd x) `elem` reductantList || (snd x ) `elem` funListIgnore then pure acc
                else do
                    funList <- getAllFunction (fromMaybe modulePath $ fst $ isLocal $ fst x) (fst x) (snd x) (mVarRedList, mvarHm) mVarModHm
                    -- writeFile ("newAST_" <> (snd x)) 
                    if fromMaybe True $ isEmpty <$> funList then do
                        redListC <- takeMVar mVarRedList
                        putMVar mVarRedList (nub $ redListC ++ [(snd x)])
                        pure $
                         if isNothing acc && isNothing funList then Nothing
                          else if isNothing acc then Just $ FlowNode (snd x) [(fromJust funList)] 
                          else if isNothing funList then acc
                          else Just $ FlowNode (getString $ fromJust acc) $ (getList $ fromJust acc) ++ [(FlowNode (snd x) ([fromJust funList]))]
                    else do
                        pure $
                         if isNothing acc && isNothing funList then Nothing
                          else if isNothing acc then Just $ (FlowNode (snd x) ([fromJust funList])) 
                          else if isNothing funList then acc
                          else Just $ FlowNode (getString $ fromJust acc) $ (getList $ fromJust acc) ++ [(FlowNode (snd x) ([fromJust funList]))] -- (acc ++ funList) 
                -- pure $ acc ++ funList
        ) Nothing y 
      pure $ 
        if isNothing a && isNothing val then Nothing
            else if isNothing a then Just $ fromJust val 
            else if isNothing val then a
            else Just $ (FlowNode (getString $ fromJust a) $ (getList $ fromJust a) ++ [fromJust val])) (Nothing) allFunList
    where
        getString (FlowNode str _) = str
        getList (FlowNode _ list) = list
        isEmpty (FlowNode _ list) = null list
        getFunAndHM = do
            hm <- readMVar mvarHm
            case HM.lookup (funGraph) (hm) of
                Just val -> pure val
                Nothing -> do
                  (allFunList) <- getFunctions modulePath moduleName funGraph mVarModHm
                  print allFunList
                  hm <- takeMVar mvarHm
                  hashM <- checkAndUpdateHashMap hm (funGraph) allFunList
                  putMVar mvarHm hashM
                  pure allFunList






checkAndUpdateHashMap :: HM.HashMap String ([[(String,String)]]) -> String -> ([[(String,String)]]) -> IO (HM.HashMap String ([[(String,String)]]))
checkAndUpdateHashMap orgHM fun funList = do -- HM.insert fun funList orgHM
    let list = filter (\x -> case HM.lookup (snd x) orgHM of
                    Just val -> any (\v -> x `elem` v)  val
                    Nothing -> True) <$> funList
    pure $ HM.insert fun list orgHM


headMaybe :: String -> [a] -> a
headMaybe err list = if null list then error $ "HEAD ERROR " <> err else head list

getFunctions :: String -> String -> String -> MVar (HM.HashMap String _) -> IO (([[(String,String)]]))
getFunctions modulePath moduleName funGraph mVarModHm = do
    (moduleAST) <- do
      modHm <- readMVar mVarModHm
      case HM.lookup moduleName modHm of
        Just ast -> pure (ast)
        Nothing -> do
            ast <- moduleParser modulePath moduleName
            modHm <- takeMVar mVarModHm
            putMVar mVarModHm $ HM.insert moduleName ast modHm
            pure (ast)
    let funDepsList = mapMaybe (\x -> traverseOverUValBind x funGraph) (moduleAST ^? biplateRef)
    case null funDepsList of
      False -> do
        let funDepList = headMaybe "funDepList" funDepsList
        let originalList = concat $ map (getRhsName') (funDepList ^? biplateRef)
        print $ originalList
        let importHM = foldl' (\acc x -> parseImportsAndGetModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
        let importHMQ = foldl' (\acc x -> parseImportsAndGetQualModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
        !allFuns <- getFunctionDepOfModule' moduleAST
        print allFuns
        let updateMods = map (\(x,y) -> case HM.lookup y allFuns of
                                        Just a -> (moduleName,y)
                                        Nothing ->
                                            case (snd <$> HM.lookup y importHM) <|> (HM.lookup y importHMQ) of
                                            Just imp -> (imp,y)
                                            Nothing -> (moduleName,y)) <$> originalList
            consideredFiles = filter (not . null) $ filter (\(x,y) -> (not $ snd $ isLocal x) && (not $ ((y) == funGraph))) <$> (updateMods)
        -- print updateMods
        writeFile ("Meta/MetaAST_" ++ funGraph) (show $ consideredFiles)
        pure $ (consideredFiles)
      True ->
        let importHM = foldl' (\acc x -> parseImportsAndGetModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
            importHMQ = foldl' (\acc x -> parseImportsAndGetQualModule x acc) HM.empty $ (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
            modN = (snd <$> HM.lookup funGraph importHM) <|> HM.lookup funGraph importHMQ
        in pure $ ((if isNothing modN then [] else filter (not . null) $ filter (\(x,y) -> (not $ snd $ isLocal x) && (not $ ((y) == funGraph))) <$> [[(fromJust modN,funGraph)]]))

getModuleName :: Ann UModuleName (Dom GhcPs) SrcTemplateStage -> Maybe String
getModuleName (Ann _ (UModuleName ex)) = Just ex
getModuleName _ = Nothing

getIEName :: Ann UIESpec (Dom GhcPs) SrcTemplateStage -> Maybe String
getIEName (Ann _ (UIESpec _ name _)) = getNamePart name
getIEName _ = Nothing

getQualifiedImportName :: AnnMaybeG UImportRenaming (Dom GhcPs) SrcTemplateStage -> Maybe String
getQualifiedImportName (AnnMaybeG _ (Just (Ann _ (UImportRenaming ex)))) = getModuleName ex
getQualifiedImportName _ = Nothing

getImportList :: AnnMaybeG UImportSpec (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getImportList (AnnMaybeG _ (Just (Ann _ (UImportSpecList (AnnListG _ list))))) =
    let newList = mapMaybe getIEName list
    in if null newList then Nothing else Just newList
getImportList _ = Nothing

parseImportsAndGetModule :: Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> HM.HashMap String (Maybe String,String) -> HM.HashMap String (Maybe String,String)
parseImportsAndGetModule expr@(Ann _ (UImportDecl _ _ _ _ modName qualifiedName specs)) hm =
    let importList = fromMaybe [] $ getImportList specs
        qualName = getQualifiedImportName qualifiedName
    in case getModuleName modName of
        Just name ->  foldl' (\acc x -> HM.insert x (qualName,name) acc) hm importList
        Nothing -> hm

parseImportsAndGetQualModule :: Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> HM.HashMap String (String) -> HM.HashMap String (String)
parseImportsAndGetQualModule expr@(Ann _ (UImportDecl _ _ _ _ modName qualifiedName specs)) hm =
    let qualName = getQualifiedImportName qualifiedName
    in case getModuleName modName of
        Just name -> maybe hm (\q -> HM.insert q name hm) qualName
        Nothing -> hm
    -- maybe hm (\x -> HM.insert x (,fromMaybe [] $ getImportList specs) hm) $ 


traverseOverUValBind :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> String -> Maybe (Ann UValueBind (Dom GhcPs) SrcTemplateStage)
traverseOverUValBind expr@(Ann _ (UValueBinding (FunctionBind' ex))) fun =
    let !funName = filter (\x -> (getFunctionNameFromValBind x) ==  Just fun) ((ex) ^? biplateRef)
    -- print funName 
    in if null funName then Nothing else Just $ mkFunctionBind'' funName
traverseOverUValBind expr@(Ann _ exp@(UValueBinding ex)) fun =
    let !funName = filter (\x -> (getFunctionNameFromSimpleBind fun x) == Just fun) (exp ^? biplateRef)
    -- print funName 
    in if null funName then Nothing else Just $ head $ funName
traverseOverUValBind expr _ =  Nothing

-- getFunctionBinds :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> String -> Maybe (Ann UMatch (Dom GhcPs) SrcTemplateStage)
-- getFunctionBinds expr@(Ann _ (UValueBinding (FunctionBind' ex))) val =
--     let !funName = filter (\x -> getFunctionNameFromValBind x == val) ((ex) ^? biplateRef)
--     -- print funName 
--     in if null funName then Nothing else Just $ head funName
-- getFunctionBinds expr _ = Nothing

getPatternName :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getPatternName expr@(Ann _ (UVarPat ex)) = Just $ mapMaybe getNamePart (ex ^? biplateRef)
getPatternName expr = Nothing

getPatternName' :: Ann UPattern (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getPatternName' expr = Just $ mapMaybe getNamePart (expr ^? biplateRef)

-- getOverMatchLhs :: Ann UMatch (Dom GhcPs) SrcTemplateStage -> String
-- getOverMatchLhs expr@(Ann _ (UMatch lhs _ _)) = getFunctionNameFromValBind lhs

getFunctionNameFromValBind :: Ann UMatch (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionNameFromValBind expr@(Ann _ (UMatch (Ann _ (UNormalLhs (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) ex1)) _ _)) = Just ex
getFunctionNameFromValBind _ = Nothing

getFunctionNameFromSimpleBind :: String -> Ann UValueBind (Dom GhcPs) SrcTemplateStage -> Maybe String
getFunctionNameFromSimpleBind str expr@(Ann _ (USimpleBind pat _ _)) = 
    let name = fromJust $ getPatternName' pat
    in if null name then Nothing else Just $ headMaybe ("getFunctionNameFromSimpleBind " <> str) name
getFunctionNameFromSimpleBind str expr@(Ann _ (UFunBind ex)) =
    let name = mapMaybe getFunctionNameFromValBind (ex ^? biplateRef)
    in if null name then Nothing else Just $ headMaybe ("getFunctionNameFromSimpleBind " <> str) name

getLhsArgs :: Ann UMatchLhs (Dom GhcPs) SrcTemplateStage -> [String]
getLhsArgs expr@(Ann _ (UNormalLhs _ args)) = mapMaybe getNamePart (args ^? biplateRef)

getNamePart :: Ann UName (Dom GhcPs) SrcTemplateStage -> Maybe String
getNamePart expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = Just ex
getNamePart expr = Nothing

getName' :: Ann UNamePart  (Dom GhcPs) SrcTemplateStage -> Maybe String
getName' (Ann _ (UNamePart name)) = Just name
getName' _ = Nothing

getNamePartWithQ :: Ann UName (Dom GhcPs) SrcTemplateStage -> Maybe (Maybe String, String)
getNamePartWithQ expr@(Ann _ (UNormalName (Ann _ (UQualifiedName (AnnListG _ names) (Ann _ (UNamePart ex)))))) =
    let name = if null names then Nothing else getName' $ head names
    in Just (name,ex)
getNamePartWithQ expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = Just (Nothing,ex)
getNamePartWithQ expr = Nothing

getLenses :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> Maybe String
getLenses expr@(Ann _ (UInfixApp _ op (Ann _ (UVar rhs)))) =
    let operator = fromMaybe "" $ getOperator op
    in if any (\x -> operator == x) [".^.", "^.", ".|.", "^.."] then getNamePart rhs else Nothing
getLenses expr = Nothing

getOperator :: Ann UOperator (Dom GhcPs) SrcTemplateStage -> Maybe String
getOperator expr@(Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = Just ex
getOperator _ = Nothing

-- getRecFieldNames :: Ann UFieldUpdate (Dom GhcPs) SrcTemplateStage -> IO (Ann UFieldUpdate (Dom GhcPs) SrcTemplateStage)
-- getRecFieldNames expr@(Ann _ (UNormalFieldUpdate ex _)) = do
--     let name = getNamePart ex
--     print name
--     print expr
--     pure expr
-- getRecFieldNames expr = do
--     print expr
--     pure expr   

getRecFieldNames :: Ann UFieldUpdate (Dom GhcPs) SrcTemplateStage -> Maybe String
getRecFieldNames expr@(Ann _ (UNormalFieldUpdate ex _)) = getNamePart ex
getRecFieldNames expr = Nothing 

getRhsName :: Ann UMatch (Dom GhcPs) SrcTemplateStage -> [(Maybe String,String)]
getRhsName expr@(Ann _ (UMatch _ rhs _)) = do
    let pats = concat $ mapMaybe getPatternName (rhs ^? biplateRef)
    let fieldNames = pats ++ (mapMaybe getRecFieldNames (rhs ^? biplateRef))
        lenses = fieldNames ++ (mapMaybe getLenses (rhs ^? biplateRef))
    let allNames = mapMaybe getNamePartWithQ (rhs ^? biplateRef)
    filter (\(y,x) -> not $ x `elem` (lenses) || (isUpper $ headMaybe "getRhsName" x)) allNames


getRhsName' :: Ann UValueBind (Dom GhcPs) SrcTemplateStage -> [[(Maybe String,String)]]
getRhsName' (Ann _ (UFunBind (AnnListG _ match))) =
  if null match then [] else
    let (Ann _ (UMatch lhs rhs _)) = head match
        args = concat $ map getLhsArgs (lhs ^? biplateRef)
        pats = concat $ mapMaybe getPatternName (rhs ^? biplateRef)
        fieldNames = pats ++ (mapMaybe getRecFieldNames (rhs ^? biplateRef))
        lenses = args ++ fieldNames ++ (mapMaybe getLenses (rhs ^? biplateRef))
    in snd $ foldl' (\acc@(range,val) x  -> let names = getNamePartFromExpr x lenses range
                                 in if snd names == Nothing then acc
                                    else (fst names, val ++ [fromMaybe [] $ snd names]) ) ([],([] :: [[(Maybe String,String)]])) (rhs ^? biplateRef :: [Ann UExpr (Dom GhcPs) SrcTemplateStage] )
getRhsName' (expr@(Ann _ (USimpleBind pat rhs _))) =
    let pats = concat $ mapMaybe getPatternName (rhs ^? biplateRef)
        fieldNames = pats ++ (mapMaybe getRecFieldNames (rhs ^? biplateRef))
        lenses =  fieldNames ++ (mapMaybe getLenses (rhs ^? biplateRef))
    in snd $ foldl' (\acc@(range,val) x  -> let names = getNamePartFromExpr x lenses range
                                 in if snd names == Nothing then acc
                                    else (fst names, val ++ [fromMaybe [] $ snd names]) ) ([],([] :: [[(Maybe String,String)]])) (rhs ^? biplateRef :: [Ann UExpr (Dom GhcPs) SrcTemplateStage] )
-- getRhsName' expr@(Ann _ (UFunBind (AnnListG _ (UMatch lhs rhs _)))) = []


-- filterBranches :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> [String] -> Maybe [(Maybe String, String)]
-- filterBranches expr@(Ann _ (UIf _ ifCond thenCond)) lenses = 
--     let ifCondNames = mapMaybe getNamePartWithQ (ifCond ^? biplateRef)
--         thenCondNames = mapMaybe getNamePartWithQ (thenCond ^? biplateRef)
--     in Just $ filter (\(y,x) -> not $ (traceShowId x) `elem` (traceShowId lenses) || (isUpper $ head x)) $ (ifCondNames ++ thenCondNames)

spanToLine :: SrcSpan -> (Int,Int)
spanToLine (RealSrcSpan s)   = (srcSpanStartLine s, srcSpanEndLine s)
spanToLine (UnhelpfulSpan _) = (0,0)

getRange' :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> (Int,Int)
getRange' expr@(Ann a _) = spanToLine $ (_sourceInfo a) ^. sourceTemplateNodeRange

isInRange :: Int -> [(Int,Int)] -> Bool
isInRange checkRange ranges =
    any (\(x,y) -> checkRange >= x && checkRange <= y) ranges

getNamePartFromExpr :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> [String] -> [(Int,Int)] -> ([(Int,Int)],Maybe [(Maybe String, String)])
getNamePartFromExpr expr@(Ann _ (UIf _ ifCond thenCond)) lenses ranges = 
    let ifCondNames = mapMaybe getNamePartWithQ (ifCond ^? biplateRef)
        thenCondNames = mapMaybe getNamePartWithQ (thenCond ^? biplateRef)
        ifRange = getRange' ifCond
        elseRange = getRange' thenCond
    in (ranges ++ [ifRange] ++ [elseRange],(Just $ filter (\(y,x) -> not $ x `elem` (lenses) || (isUpper $ headMaybe "getNamePartFromExpr" x)) $ (ifCondNames ++ thenCondNames)))
getNamePartFromExpr expr@(Ann a (UMultiIf (alts))) lenses ranges = 
    let ifCondNames = mapMaybe getNamePartWithQ (alts ^? biplateRef)
        ifRange = getRange' expr
    in (ranges ++ [ifRange],(Just $ filter (\(y,x) -> not $ x `elem` (lenses) || (isUpper $ headMaybe "getNamePartFromExpr" x)) $ (ifCondNames)))
getNamePartFromExpr expr@(Ann a (UCase ex (alts))) lenses ranges = 
    let ifCondNames = mapMaybe getNamePartWithQ (alts ^? biplateRef)
        ifRange = getRange' expr
    in (ranges ++ [ifRange],(Just $ filter (\(y,x) -> not $ x `elem` (lenses) || (isUpper $ headMaybe "getNamePartFromExpr" x)) $ (ifCondNames)))
getNamePartFromExpr expr@(Ann _ (UVar _)) lenses ranges =
  let range = getRange' expr
  in 
    if isInRange (fst range) ranges then (ranges,Nothing)
    else
        let val = filter (\(y,x) -> not $ x `elem` (lenses) || (isUpper $ headMaybe "getNamePartFromExpr" x)) $ mapMaybe getNamePartWithQ (expr ^? biplateRef)
        in if null val then (ranges,Nothing) else (ranges,Just val)
getNamePartFromExpr _ lenses ranges = (ranges,Nothing)
-- getNamePartFromExpr expr@(Ann _ (UVar _)) _ = Nothing
-- getNamePartFromExpr expr@(Ann _ (UParen _)) _= Nothing
-- getNamePartFromExpr expr lenses =
--   let val = filter (\(y,x) -> not $ x `elem` (lenses) || (isUpper $ head x)) $ mapMaybe getNamePartWithQ (expr ^? biplateRef)
--   in if null val then Nothing else Just val

toDataTree :: FlowTree -> Tree String
toDataTree (FlowNode name children) = Node name (map toDataTree children)

treeToString :: FlowTree -> String
treeToString (FlowNode name children) = name ++ "\n" ++ concatMap (\child -> "  " ++ treeToString child) children