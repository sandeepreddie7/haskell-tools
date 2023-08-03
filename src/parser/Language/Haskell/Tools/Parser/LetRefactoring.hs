module Language.Haskell.Tools.Parser.LetRefactoring where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint
import Data.List
import GHC
import Control.Reference
import Language.Haskell.Tools.Parser.ParseModule

removeMultiLets :: String -> String -> IO ()
removeMultiLets modulePath moduleName = do
    moduleAST <- moduleParser modulePath moduleName
    newAST <- (!~) (biplateRef) (letRefactor) (moduleAST)
    writeFile moduleName (prettyPrint newAST)

-- Get all let statements, Group and Refactor
letRefactor :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UDecl (Dom GhcPs) SrcTemplateStage)
letRefactor expr@(Ann _ (UValueBinding sim@(Ann _ (USimpleBind pat rhs@(Ann _ (UUnguardedRhs y@(Ann _ ((UDo _ stmts@(AnnListG _ ex)))))) (AnnMaybeG _ binds))))) =
    let groupedStmts = groupBy (\a b -> checkIfLet a && checkIfLet b) ex
        modifiedStmts = concat $ map modifiyStmts groupedStmts
        doBlock = mkDoBlock' modifiedStmts
        makeRhs = mkUnguardedRhs' doBlock
        simpleBind = mkSimpleBinds pat makeRhs binds
    in pure $ mkValueBinding' simpleBind

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

{- Example code:

getAllNames = do
  let a = "hi"
  let b = "hello"
  let c = "mike"
  d <- pure "bbb"
  let h = "in"
  let k = "lll"
  a <> b <> c <> h <> k

Refactored code

getAllNames = do let a = "hi"
                     b = "hello"
                     c = "mike"
                 d <- pure "bbb"
                 let h = "in"
                     k = "lll"
                 a <> b <> c <> h <> k -}