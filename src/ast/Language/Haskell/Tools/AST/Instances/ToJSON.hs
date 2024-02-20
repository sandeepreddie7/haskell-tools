{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Language.Haskell.Tools.AST.Instances.ToJSON where


import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Instances.Data ()
import Language.Haskell.Tools.AST.Instances.Generic ()

import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Kinds
import Language.Haskell.Tools.AST.Representation.Literals
import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Stmts
import Language.Haskell.Tools.AST.Representation.TH
import Language.Haskell.Tools.AST.Representation.Types
import Data.Eq.Deriving (deriveEq)

instance (DomainWith e dom, SourceInfo stage, Eq (e dom stage))
      => Eq (AnnMaybeG e dom stage) where

instance (DomainWith e dom, SourceInfo stage, Eq (e dom stage))
      => Eq (AnnListG e dom stage) where

deriving instance (Domain dom, SourceInfo stage) => Eq (UModule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UModuleHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UExportSpecs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UExportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UIESpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (USubSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UModulePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UFilePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UImportDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UImportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UImportModifier dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UImportQualified dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UImportSource dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UImportSafe dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UTypeNamespace dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UImportRenaming dom stage)

-- Declarations
deriving instance (Domain dom, SourceInfo stage) => Eq (UDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UClassBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UClassElement dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UDeclHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UInstBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UInstBodyDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UGadtConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UGadtConType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UFieldWildcard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UFunDeps dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UFunDep dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UFieldDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UDeriving dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UDeriveStrategy dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UInstanceRule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UInstanceHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UTypeEqn dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UKindConstraint dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UTyVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UContext dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UAssertion dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UExpr dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Eq (expr dom stage)) => Eq (UStmt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UCompStmt dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UValueBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UPattern dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UPatternField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (USplice dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (QQString dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UMatch dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Eq (expr dom stage)) => Eq (UAlt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (URhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UGuardedRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UFieldUpdate dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UBracket dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UTopLevelPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (URule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (URuleVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UAnnotationSubject dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UMinimalFormula dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UExprPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (USourceRange dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (Number dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UQuasiQuote dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (URhsGuard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (ULocalBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (ULocalBinds dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UFixitySignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UListCompBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UTupSecElem dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UTypeFamily dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UTypeFamilySpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UInjectivityAnn dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Eq (expr dom stage)) => Eq (UCaseRhs' expr dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Eq (expr dom stage))=> Eq (UGuardedCaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UPatternSynonym dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UPatSynRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UPatSynLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UPatSynWhere dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UPatternTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (URole dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UCmd dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (ULanguageExtension dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UMatchLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UInlinePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (USpecializePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UUnboxedSumPlaceHolder dom stage)

-- ULiteral
deriving instance (Domain dom, SourceInfo stage) => Eq (ULiteral dom stage)
deriving instance (DomainWith k dom, SourceInfo stage, Eq (k dom stage)) => Eq (UPromoted k dom stage)

-- Base
deriving instance (Domain dom, SourceInfo stage) => Eq (UOperator dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UQualifiedName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UModuleName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UNamePart dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UStringNode dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UDataOrNewtypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UDoKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (TypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UOverlapPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UCallConv dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UArrowAppl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (USafety dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UConlikeAnnot dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (Assoc dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (Precedence dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (LineNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (UPhaseControl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (PhaseNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Eq (PhaseInvert dom stage)
