{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , TemplateHaskell
           , DeriveDataTypeable
           , StandaloneDeriving
           #-}
-- | Parts of AST representation for keeping extra data
module Language.Haskell.Tools.AST.Ann where

import Data.Data
import Control.Reference
import SrcLoc
import Name
import Module
import Id

-- | An element of the AST keeping extra information.
data Ann elem annot
-- The type parameters are organized this way because we want the annotation type to
-- be more flexible, but the annotation is the first parameter because it eases 
-- pattern matching.
  = Ann { _annotation :: annot -- ^ The extra information for the AST part
        , _element    :: elem annot -- ^ The original AST part
        }
        
makeReferences ''Ann
        
-- | Semantic and source code related information for an AST node.
data NodeInfo sema src 
  = NodeInfo { _semanticInfo :: sema
             , _sourceInfo :: src
             }
  deriving (Eq, Show, Data)
             
makeReferences ''NodeInfo

-- | Location info for different types of nodes
data SpanInfo 
  = NodeSpan { _nodeSpan :: SrcSpan }
  | ListPos { _listDefaultSep :: String
            , _listPos :: SrcLoc 
            }
  | OptionalPos { _optionalBefore :: String
                , _optionalAfter :: String 
                , _optionalPos :: SrcLoc 
                }
  deriving (Eq, Show)
  
makeReferences ''SpanInfo

-- | Extracts the concrete range corresponding to a given span.
-- In case of lists and optional elements, it may not contain the elements inside.
spanRange :: SpanInfo -> SrcSpan
spanRange (NodeSpan sp) = sp
spanRange (ListPos _ pos) = srcLocSpan pos
spanRange (OptionalPos _ _ pos) = srcLocSpan pos
  
class HasRange annot where
  getRange :: annot -> SrcSpan
  
instance HasRange (NodeInfo sema SpanInfo) where 
  getRange = spanRange . (^. sourceInfo)
  
type RangeInfo = NodeInfo () SpanInfo
type RangeWithName = NodeInfo (SemanticInfo Name) SpanInfo
type RangeWithType = NodeInfo (SemanticInfo Id) SpanInfo

-- | Semantic information for an AST node. Semantic information is
-- currently heterogeneous.
data SemanticInfo n
  = NoSemanticInfo -- ^ Semantic info type for any node not 
                   -- carrying additional semantic information
  | NameInfo { _nameInfo :: n 
             } -- ^ Info corresponding to a name
  | OnlyNameInfo { _onlyNameInfo :: Name 
                 } -- ^ Info of a name that has no accessible type
  | ImportInfo { _importedModule :: Module -- ^ The name and package of the imported module
               , _availableNames :: [n] -- ^ Names available from the imported module
               , _importedNames :: [n] -- ^ Names actually imported from the module.
               } -- ^ Info corresponding to an import declaration
  -- | ImplicitImports [ImportDecl]
  -- | ImplicitFieldUpdates [ImportDecl]
  deriving (Eq, Data)
    
makeReferences ''SemanticInfo

-- | A list of AST elements
data AnnList e a = AnnList { _annListAnnot :: a 
                           , _annListElems :: [Ann e a]
                           }
                           
makeReferences ''AnnList
        
annList :: Traversal (AnnList e a) (AnnList e' a) (Ann e a) (Ann e' a)                          
annList = annListElems & traversal

-- | An optional AST element
data AnnMaybe e a = AnnMaybe { _annMaybeAnnot :: a 
                             , _annMaybe :: (Maybe (Ann e a))
                             }
                             
makeReferences ''AnnMaybe
                          
annJust :: Partial (AnnMaybe e a) (AnnMaybe e' a) (Ann e a) (Ann e' a)                          
annJust = annMaybe & just

-- | An empty list of AST elements
annNil :: a -> AnnList e a
annNil a = AnnList a []

isAnnNothing :: AnnMaybe e a -> Bool
isAnnNothing (AnnMaybe _ Nothing) = True
isAnnNothing (AnnMaybe _ _) = False

-- | A non-existing AST part
annNothing :: a -> AnnMaybe e a
annNothing a = AnnMaybe a Nothing

  
class HasAnnot node where
  getAnnot :: node a -> a

instance HasAnnot (Ann e) where
  getAnnot = (^. annotation)
  
instance HasAnnot (AnnList e) where
  getAnnot = (^. annListAnnot)
  
instance HasAnnot (AnnMaybe e) where
  getAnnot = (^. annMaybeAnnot)
