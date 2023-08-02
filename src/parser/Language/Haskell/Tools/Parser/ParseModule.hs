{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Language.Haskell.Tools.Parser.ParseModule where
  
-- import Retrie
import Debug.Trace
import Data.Data
import GHC hiding (loadModule)
import qualified GHC
import Outputable (Outputable(..), showSDocUnsafe, cat)
-- import GHC.Unit.Module.Graph (mgModSummaries')
import GHC.Paths ( libdir )
import Control.Monad
-- import DynFlags
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
-- import AliasedTypes (ModuleMeta(..))
import Data.List
import Data.List.Extra (splitOn,trim,replace, cons)
import GHC.LanguageExtensions
-- import qualified Language.Haskell.GHC.ExactPrint as E
-- import qualified ProjectModuleGraph
import Control.Exception
import Data.Functor
import Data.Maybe
import System.Directory
import Debug.Trace (traceShowId)
import Shelly
-- import GHC.Types.SrcLoc
-- import GHC.Parser.Annotation
import Control.Concurrent
import SrcLoc (noSrcSpan, combineSrcSpans)

-- import DynFlags
-- import Data.Maybe
-- import Lexer
-- import FastString
-- import SrcLoc
-- import HscTypes
-- import HscMain
import DynFlags
import Language.Haskell.Tools.BackendGHC
import Language.Haskell.Tools.PrettyPrint.Prepare
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Decls (trfDecls, trfDeclsGroup)
import Language.Haskell.Tools.BackendGHC.Exprs (trfText')
import Language.Haskell.Tools.BackendGHC.Names (TransformName, trfName)
import Language.Haskell.Tools.BackendGHC.Modules hiding (trfModuleHead)
import Language.Haskell.Tools.AST
-- import Language.Haskell.Tools.Debug.RangeDebug
-- import Language.Haskell.Tools.BackendGHC.Monad

-- import qualified Parser as P
-- import Bag
-- import ErrUtils

useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }

initGhcFlags :: Ghc ()
initGhcFlags = initGhcFlags' False True

initGhcFlags' :: Bool -> Bool -> Ghc ()
initGhcFlags' needsCodeGen errorsSuppressed = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ (if errorsSuppressed then flip gopt_set Opt_DeferTypeErrors
                                  . flip gopt_set Opt_DeferTypedHoles
                                  . flip gopt_set Opt_DeferOutOfScopeVariables
                           else id)
    $ foldl' (\acc x -> xopt_set acc x) (dflags { importPaths = []
            --  , hscTarget = if needsCodeGen then HscInterpreted else HscNothing
             , ghcLink = if needsCodeGen then LinkInMemory else NoLink
             , ghcMode = CompManager
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
            --  , pluginModNames = pluginModNames dflags ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
             }) [
                                BangPatterns
                                ,BlockArguments
                                ,ConstraintKinds
                                -- ,Cpp
                                ,DataKinds
                                ,DeriveAnyClass
                                ,DeriveDataTypeable
                                ,DeriveFoldable
                                ,DeriveFunctor
                                ,DeriveGeneric
                                ,DeriveTraversable
                                ,DerivingStrategies
                                ,DerivingVia
                                ,DuplicateRecordFields
                                ,EmptyCase
                                ,ExplicitForAll
                                ,ExplicitNamespaces
                                ,FlexibleContexts
                                ,FlexibleInstances
                                ,GADTs
                                ,GeneralizedNewtypeDeriving
                                ,ImplicitParams
                                ,ImplicitPrelude
                                ,InstanceSigs
                                ,KindSignatures
                                ,LambdaCase
                                ,MagicHash
                                ,MultiParamTypeClasses
                                ,MultiWayIf
                                ,OverloadedLabels
                                ,OverloadedStrings
                                ,PatternSynonyms
                                ,QuasiQuotes
                                ,RankNTypes
                                ,RecordWildCards
                                ,ScopedTypeVariables
                                ,TemplateHaskell
                                ,TupleSections
                                ,TypeApplications
                                ,TypeFamilies
                                ,TypeOperators
                                ,TypeSynonymInstances
                                ,UndecidableInstances
                                ,ViewPatterns
                                ,GHC.LanguageExtensions.UnicodeSyntax
                          ]

initGhcFlagsForTest :: Ghc ()
initGhcFlagsForTest = do initGhcFlags' True False
                         dfs <- getSessionDynFlags
                         void $ setSessionDynFlags dfs  
                        --  {
                        --   extensions = 
                        --  }
                        --  { hscTarget = HscAsm
                        --                                 --  , pluginModNames = pluginModNames dfs ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
                        --                                   }

loadModule :: FilePath -> String -> Ghc ModSummary
loadModule workingDir moduleName
  = do initGhcFlagsForTest
       useDirs [workingDir]
       target <- guessTarget moduleName Nothing
       setTargets [target]
       void $ load (LoadUpTo $ mkModuleName moduleName)
       getModSummary $ mkModuleName moduleName
  -- = do  initGhcFlagsForTest
  --       useDirs [workingDir]
  --       target <- guessTarget moduleName Nothing
  --       setTargets [target]
  --       void $ load (LoadUpTo $ mkModuleName moduleName)
  --       (_errs, modGraph) <- depanalE [] False
  --       let s =  ppr $ topSortModuleGraph False modGraph Nothing
  --       liftIO $ writeFile "aaaaaa.log" $ show s
  --       getModSummary $ mkModuleName moduleName


-- getModuleExports :: MonadInterpreter m => ModuleName -> m [ModuleElem]
-- getModuleExports mn =
--     do module_  <- findModule mn
--        mod_info <- mayFail $ runGhc $ GHC.getModuleInfo module_
--        exports  <- mapM (\n -> runGhc $ GHC.lookupName n) (GHC.modInfoExports mod_info)
--        dflags   <- runGhc GHC.getSessionDynFlags
--        --
--        return $ asModElemList dflags (catMaybes exports)

-- convert :: _ -> 
-- convert (GenLocated _ (ImportDecl 
--       ideclExt        -- :: XCImportDecl pass,
--       ideclName                                     -- :: XRec pass ModuleName, -- ^ Module name.
--       ideclPkgQual                                  -- :: ImportDeclPkgQual pass,  -- ^ Package qualifier.
--       ideclSource                                   -- :: IsBootInterface,      -- ^ IsBoot <=> {-\# SOURCE \#-} import
--       ideclSafe                                     -- :: Bool,          -- ^ True => safe import
--       ideclQualified                                -- :: ImportDeclQualifiedStyle, -- ^ If/how the import is qualified.
--       ideclAs                                       -- :: Maybe (XRec pass ModuleName),  -- ^ as Module
--       ideclImportList)) = 
--         case ideclQualified of
--           QualifiedPre -> if isJust ideclAs then QualifiedImport else QualifiedImport
--           QualifiedPost -> if isJust ideclAs then QualifiedImport  else QualifiedImport
--           NotQualified ->  if isJust ideclImportList then fromJust  else
--         if isJust ideclAs ||  ideclName ideclPkgQual ideclQualified ideclAs ideclImportList                -- :: Maybe (ImportListInterpretation, XRec pass [LIE pass])
--                                        -- ^ Explicit import list (EverythingBut => hiding, names)))

pp = "/home/chaitanya/Desktop/work/"
-- isHiding :: ImportListInterpretation -> Bool
-- isHiding Exactly = False
-- isHiding EverythingBut = True

-- instance Outputable ModuleGraph where
--     ppr = pprStackInfo

foldLocs :: [SrcSpan] -> SrcSpan
foldLocs = foldl combineSrcSpans noSrcSpan

-- trfModuleHead :: TransformName n r
--               => Maybe (Located ModuleName) -> SrcLoc -> Maybe (Located [LIE n])
--                    -> Maybe (Located WarningTxt) -> Trf (AnnMaybeG AST.UModuleHead (Dom r) RangeStage)
-- trfModuleHead (Just mn) _ exports modPrag
--   = makeJust <$> (annLocNoSema (tokensLoc [AnnModule, AnnWhere])
--                                (AST.UModuleHead <$> trfModuleName mn
--                                                 <*> trfModulePragma (srcSpanEnd $ getLoc mn) modPrag
--                                                 <*> trfExportList (before AnnWhere) exports))
-- trfModuleHead _ rng Nothing _ = nothing "" "" (pure rng)
-- trfModuleHead Nothing _ (Just _) _ = convertionProblem "trfModuleHead: no head but has exports"
moduleParser :: String -> String -> IO ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage))
moduleParser modulePath moduleName = do
-- x <- readFile (pp <> "euler-api-txns/ecPrelude/src/Nau/Utils/DecodeField.hs")(replace "/home/chaitanya/Desktop/work/euler-api-txns/" modulePath)
  -- runGhc (Just libdir) $ do
    print modulePath
    dflags <- runGhc (Just libdir) getSessionDynFlags
    modSum <- runGhc (Just libdir) $ loadModule (if "src-generated" `isInfixOf` modulePath then pp <> "euler-api-txns/euler-x/src-generated/" else if "src-extras" `isInfixOf` modulePath then pp <> "euler-api-txns/euler-x/src-extras/" else pp <> "euler-api-txns/euler-x/src/") moduleName
    print $ showSDocUnsafe $ ppr modSum
    y <- runGhc (Just libdir) $ parseModule modSum
    let annots = pm_annotations y
    valsss <- runGhc (Just libdir) $ runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule' modSum (pm_parsed_source y)
    sourceOrigin <- return (fromJust $ ms_hspp_buf $ pm_mod_summary y)
    newAst <- runGhc (Just libdir) $ (prepareAST) sourceOrigin . placeComments (fst annots) (getNormalComments $ snd annots)
        <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule' modSum $ pm_parsed_source y)
    -- print valsss
    --   srcBuffer <- if hasCppExtension
    --                 then liftIO $ hGetStringBuffer (getModSumOrig ms)
    --                 else return (fromJust $ ms_hspp_buf $ pm_mod_summary p)
    -- liftIO $ print newAst
    pure newAst
        -- (\(HsModule name exports imports decls deprec _) ->
        --     AST.UModule <$> trfFilePragmas
        --                 <*> trfModuleHead name (srcSpanStart (foldLocs (map getLoc imports ++ map getLoc decls))) exports deprec
        --                 <*> trfImports imports
        --                 <*> trfDecls decls) $ ((unLoc $ pm_parsed_source y))
    -- mod <- I.runInterpreter $ do
    --   -- I.setImports ["Prelude", "Data.Typeable", "DynFlags"]
    --   I.getModuleExports "Prelude"
    -- print mod
    -- print (showSDocUnsafe $ ppr $ modSum)
    -- let buf = (show $ fromJust $ ms_hspp_buf modSum)
    -- modSum <- runGhc (Just libdir) $ getModSummary (mkModuleName "ECPrelude.hs")

        -- let (y,y1) = (getMessages pst dflags)
        -- print $ bagToList y1
        -- print $ bagToList y
          -- tc <- typecheckModule y
    -- void $ GHC.loadModule tc -- when used with loadModule, the module will be loaded twice
    -- let (functions, nonFunctions) = foldl' (\acc res -> do 
    --         if isFunction res
    --           then ((fst acc <> [(unLoc res)]),snd acc)
    --           else ((fst acc),snd acc <> [(unLoc res)])
    --         ) ([],[]) (hsmodDecls $ unLoc $ pm_parsed_source y)
    --     groupedFunctions = groupBy (\a b -> isFunSig a && isFunVal b ) functions
    --     groupFunctionsMore = groupByUltimate groupedFunctions --unionBy (\a b -> getInfixlFun a b || getInfixlFun b a) (groupedFunctions)
    --     importList = unLoc <$> (hsmodImports $ unLoc $ pm_parsed_source y)
    -- -- print $ showSDocUnsafe $ ppr groupedFunctions
    -- -- print $ showSDocUnsafe $ ppr groupFunctionsMore
    -- pure $ (groupFunctionsMore,nonFunctions,importList, pm_parsed_source y)

    -- where
    --   getInfixlFun fun fun1 =
    --     let funShow = replace "]" "" $ replace "[" "" $ showSDocUnsafe $ ppr fun
    --         funShow1 = showSDocUnsafe $ ppr fun1
    --     in if any (\x -> isInfixOf x funShow) ["infixr", "infixl", "infix"]  then any (\x -> isInfixOf (trim $ traceShowId x) $ traceShowId $ funShow1) (traceShowId $ splitOn " " funShow) else False
    -- liftIO $ print (map fun $ hsmodDecls $ unLoc $ pm_parsed_source $ tm_parsed_module tc)
    -- liftIO $ print ((\(gr,_,_,_) -> map fun1 $ hs_warnds gr) $ fromJust $ tm_renamed_source tc)
--   runScript $ \opts -> do
--   [rewrite] <- parseRewrites opts [Adhoc "forall f x xs g a s. f (\\x -> x ^. xs) (g (a s)) = f (xs) (g (Just s))"]
--   return $ apply [setRewriteTransformer stringToFooArg rewrite]
-- fun1 ((warnDecls))  = showSDocUnsafe $ ppr warnDecls
--   -- WarningD _ (Warnings _ _ [L _ (Warning _ names (DeprecatedTxt _ stringLits))])
-- -- argMapping :: FastString -> String
-- -- argMapping val = "StorageType." <> (snd $ splitAt 3 $ unpackFS val)

isFunction :: _ -> Bool
isFunction (L _ (SigD _ sigDecls)) = True
isFunction (L _ (ValD _ valDecls)) = True
isFunction _ = False

isFunSig :: _ -> Bool
isFunSig (SigD _ sigDecls) = True
isFunSig _ = False

isFunVal :: _ -> Bool
isFunVal (ValD _ valDecls) = True
isFunVal _ = False

-- fun :: _ -> String
-- -- fun (L _ (WarningD _ warnDecls)) = showSDocUnsafe $ ppr warnDecls
-- -- fun (L _ (WarningD _ (Warnings _ _ [L _ (Warning _ names x@(DeprecatedTxt _ stringLits))]))) =
-- fun (L _ (SigD _ sigDecls)) =  showSDocUnsafe $ ppr sigDecls
-- fun (L _ (ValD _ valDecls)) =  showSDocUnsafe $ ppr valDecls
-- -- fun (L _ (TyClD _ tyDecls)) = showSDocUnsafe $ ppr tyDecls
-- -- fun _ =  mempty

-- stringToFooArg :: MatchResultTransformer
-- stringToFooArg _ctxt match
--   | MatchResult substitution template <- match
--   , Just (HoleExpr expr) <- traceShowId $ lookupSubst "xs" substitution 
--   , L _ b@(HsVar x p) <- astA expr = do
--     let !k = traceShowId $ rdrFS $ GHC.unLoc p
--     newExpr <- parseExpr $ traceShowId $ argMapping k
--     return $
--       MatchResult (extendSubst substitution "xs" (HoleExpr newExpr)) template
--   | otherwise = return NoMatch


-- runRecordDotToLens :: FilePath -> IO ()
-- runRecordDotToLens tdir = runScriptWithModifiedOptions libdir rewriter
--   where
--         rewriter :: Options -> IO (Options, Retrie ())
--         rewriter opts = do
--                     imports <- parseImports libdir ["import Types.Lenses as L"]
--                     rewrites <- parseRewrites libdir opts [
--                         Adhoc "forall f g h. f.g.h.i = (f ^. (g . h . i))", 
--                         Adhoc "forall f g h. f.g.h = (f ^. (g . h))", 
--                         Adhoc "forall f g. f.g = (f ^. g)"
--                       ]
--                     let opts' = opts {
--                         targetDir       = tdir
--                       , singleThreaded  = True
--                       , extraIgnores    = targetDirIgnoreFiles
--                       , iterateN        = 3
--                       -- , fixityEnv       = newF
--                       -- , executionMode   = ExecDryRun
--                       , executionMode   = ExecRewrite
--                       , targetFiles     = targetDirFiles
--                       -- , verbosity       = Loud
--                       }
--                     return (opts', ifChanged (apply $ fmap (setRewriteTransformer recordDotTransformer) rewrites) (addImports imports))
getFunctionName :: _ -> String
getFunctionName str = 
  if any (\x -> x `isInfixOf` str) ["infixl", "infixr", "infix", "INLINE", "NOINLINE"]
    -- infixl 7 /
    then (splitOn " " $  replace "]" "" $ replace "[" "" str) !! 2
    else head . splitOn " " . replace "(" "" . replace ")" "" . replace "]" "" $ replace "[" "" $ str

-- getDef :: _ -> String
-- getDef _ = ""

-- checkIfCyclicCall :: [_] -> [_] -> Bool
-- checkIfCyclicCall target destination = 
--   let tf = getFunctionName target
--       df = getFunctionName destination
--   in (tf `isInfixOf` (getDef destination)) && (df `isInfixOf` (getDef target))

groupByUltimate :: [[HsDecl GhcPs]] -> [(String,[[HsDecl GhcPs]])]
groupByUltimate = (HM.toList . foldl' (\acc x -> addToBucket acc x) HM.empty)
  where
    addToBucket :: HM.HashMap String [[HsDecl GhcPs]] -> [HsDecl GhcPs] -> HM.HashMap String [[HsDecl GhcPs]]
    addToBucket acc el = 
      let funcName = getFunctionName $ showSDocUnsafe $ ppr $ el
      in (HM.insert funcName $ 
              case HM.lookup funcName acc of
                Just x -> x ++ [el]
                _ -> [el]
          ) acc

-- getImportsFromProcessedFiles :: String -> _
-- getImportsFromProcessedFiles path = do
--     x <- readFile path
--     let y =  map (\x -> "import " <> x) $ filter (/= "") $ splitOn "import" x
--     imports <- runGhc (Just libdir) $ (sequence $ map parseImportDecl y)
--     pure (filter nonModuleImports imports)
--     -- print $ ppr (filter nonModuleImports imports)
--   where
--     nonModuleImports x@(ImportDecl _ _ modName pkgQual isBoot _ isQual _ asMod (Just (importList,ims))) = do
--         ppr ims /= "[]"

-- parseModuleAndSetImports :: String -> String -> String -> _
-- parseModuleAndSetImports filePath parsedFile moduleName = do
--     res <- try $ moduleParser filePath moduleName :: IO (Either SomeException _)
--     case res of
--       Right (x, y, z, parsedSource) -> do
--         eImports <- try $ getImportsFromProcessedFiles parsedFile :: IO (Either SomeException _)
--         case eImports of
--           Right imports -> do
--             let ps = unLoc parsedSource
--             if length (hsmodImports ps) > 0 
--               then 
--                 let lastImport = getLoc $ last (hsmodImports ps)
--                     firstImport = getLoc $ head (hsmodImports ps)
--                     firstImportLine = 
--                       case firstImport of
--                         (SrcSpanAnn _ (RealSrcSpan l _)) -> srcSpanStartLine l
--                         _ -> -1
--                     lastImportLine = 
--                       case lastImport of
--                         (SrcSpanAnn _ (RealSrcSpan l _)) -> srcSpanEndLine l
--                         _ -> -1
--                     allImports = zip (imports) (hsmodImports ps)
--                     xRecImports = map (\(x,y) -> (mapLoc (const x) y)) allImports
--                 in pure (firstImportLine,lastImportLine,imports)
--               else do
--                 pure $ (-1,-1,[])
--           Left err -> do
--             writeFile ("error" <> moduleName <>".log") ((show err) <> ":" <> filePath)
--             pure $ (-1,-1,[])           
--       Left err -> do
--         writeFile ("error" <> moduleName <>".log") ((show err) <> ":" <> filePath)
--         pure $ (-1,-1,[])


-- runHlint :: String -> IO ()
-- runHlint  newFileName = do
--     -- result <- shelly $ bash ("ormolu --mode inplace " <> newFileName) []
--     -- result <- shelly $ bash ("hlint --refactor --refactor-options=-i " <> newFileName) []
--     pure ()

-- replaceWithGHCParsedImports :: String -> String -> IO ()
-- replaceWithGHCParsedImports pathToParsedImports pathToSourceCode = do
--   modules <- ProjectModuleGraph.getAllHaskellModules pathToSourceCode
--   print $ length modules
--   mapM_ (\modulePath -> forkIO $ do
--       fileData <- readFile modulePath
--       let moduleName = fromMaybe "" $ ProjectModuleGraph.getModuleName fileData
--       isParsedImportExist <- doesFileExist (pathToParsedImports <> moduleName <> ".imports")
--       when (isParsedImportExist) $ do
--           (firstImportLine,lastImportLine,imports) <- parseModuleAndSetImports modulePath (pathToParsedImports <> moduleName <> ".imports") moduleName
--           when ((length imports /= 0) || firstImportLine /= -1 || lastImportLine /= -1) $
--             let refinedImportsList = map trim $ map (E.exactPrint) imports
--                 prifixData = fst $ splitAt (firstImportLine - 1)  (lines fileData)
--                 suffixData = snd $ splitAt (lastImportLine) (lines fileData)
--                 completeFileData = prifixData <> refinedImportsList <> suffixData
--             in (removeFile modulePath *> writeFile modulePath (unlines completeFileData) *> pure ())
--           ) modules
--   pure ()