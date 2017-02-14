-----------------------------------------------------------------------------
--
-- Module      :  CSP.Coherence.Context
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CSP.Coherence.Context(

-- * Definitions

  Context(..), CtxRelations(..), CtxRelations'(..)

, ContextRelation(..)
, SomeContextRelation(..), SomeRelationDetails(..)

, FilteringContext(..)

-- * Relations

, CtxBinaryRelation(..)
, CtxWholeRelation(..)


-- * Misc

, GenericContext(..), assessAtCxt

, Id(..), fromId

, module CSP.Coherence.Information

) where

import CSP.Coherence.Information

import Data.Maybe (maybeToList, mapMaybe)

import qualified Data.Set as Set

import Control.Arrow (first, second)

-----------------------------------------------------------------------------

-- | A context forms an information graph from the local
--   and assessed information sets, that become its nodes.
--   The edges are formed by context-defined relations, that
--   could be pure or IO, depending on `CtxRelsM` type.

class Context c a | c -> a
  where
    type CtxDataM c :: * -> *
    type CtxRelsM c :: * -> *

    type CtxDetails c :: *
    type CtxMode    c :: *

    ctxName      :: c -> String
    ctxData      :: c -> CtxDataM c Information
    ctxRelations :: c -> CtxRelations (CtxMode c) (CtxRelsM c) (CtxDetails c) a


class CtxRelations' rels mode m d a | rels -> mode, rels -> m, rels -> d, rels -> a
  where
    ctxRelations'  :: rels -> [SomeContextRelation mode m a]
    ctxCombineRels :: rels -> [(a, SomeRelationDetails)] -> (a, d)

data CtxRelations mode m d a = forall rels . CtxRelations' rels mode m d a =>
     CtxRelations rels

instance CtxRelations' (CtxRelations mode m d a) mode m d a where
  ctxRelations'  (CtxRelations r) = ctxRelations' r
  ctxCombineRels (CtxRelations r) = ctxCombineRels r

-----------------------------------------------------------------------------

-- | A context could be used for filtering coherent information.
class (Context c a) => FilteringContext c a | c -> a
  where
    type CtxThreM c :: * -> *
    ctxThreshold :: c -> CtxThreM c a

    type CtxFilterM c :: * -> *
    isCoherentAtCtx   :: c -> CtxMode c -> Information
                      -> CtxFilterM c (Bool, (a, CtxDetails c))
    isCoherentAtCtxIO :: c -> CtxMode c -> Information
                      -> IO (Bool, (a, CtxDetails c))


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


class ContextRelation rel mode m a | rel -> mode, rel -> m, rel -> a
  where
    type RelationDetails rel :: *
    relationName   :: rel -> String
    assessRelation :: rel -> mode -> Information -> Maybe (m (a, RelationDetails rel))

data SomeContextRelation mode m a = forall rel . ( ContextRelation rel mode m a
                                                --  , RelationDetails rel ~ SomeRelationDetails
                                                 ) =>
     SomeContextRelation rel

instance (Functor m) => ContextRelation (SomeContextRelation mode m a) mode m a where
  type RelationDetails (SomeContextRelation mode m a) = SomeRelationDetails
  relationName   (SomeContextRelation rel) = relationName rel
  assessRelation (SomeContextRelation rel) mode =
    fmap (fmap (second (SomeRelationDetails rel))) . assessRelation rel mode

data SomeRelationDetails = forall rel mode m a . ContextRelation rel mode m a =>
     SomeRelationDetails rel (RelationDetails rel)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- * Relations

data CtxBinaryRelation mode m d a = forall d' . CtxBinaryRelation {
    ctxBinRelName     :: String
  , ctxBinRel         :: mode
                      -> SomeInformationPiece
                      -> SomeInformationPiece
                      -> Maybe (m (a, d'))
  , ctxBinRelsCombine :: (a, d') -> (a, d) -> (a, d)
  , emptyBinaryRelRes :: (a, d)
  }

data CtxBinaryRelation' mode m a =
    forall d . ContextRelation (CtxBinaryRelation mode m d a) mode m a =>
    CtxBinaryRelation' (CtxBinaryRelation mode m d a)

instance (Functor m) => ContextRelation (CtxBinaryRelation' mode m a) mode m a where
  type RelationDetails (CtxBinaryRelation' mode m a) = SomeRelationDetails
  relationName   (CtxBinaryRelation' rel) = relationName rel
  assessRelation (CtxBinaryRelation' rel) = assessRelation' rel

assessRelation' rel mode = fmap (fmap (second $ SomeRelationDetails rel))
                    . assessRelation rel mode


-----------------------------------------------------------------------------

data CtxWholeRelation mode m d a = CtxWholeRelation {
    ctxWholeRelName :: String
  , ctxWholeRel     :: mode -> Information -> Maybe (m (a, d))
  }

data CtxWholeRelation' mode m a =
    forall d . ContextRelation (CtxWholeRelation mode m d a) mode m a =>
    CtxWholeRelation' (CtxWholeRelation mode m d a)

instance (Functor m) => ContextRelation (CtxWholeRelation' mode m a) mode m a where
  type RelationDetails (CtxWholeRelation' mode m a) = SomeRelationDetails
  relationName   (CtxWholeRelation' rel) = relationName rel
  assessRelation (CtxWholeRelation' rel) = assessRelation' rel



-----------------------------------------------------------------------------

instance (Monad m) =>
  ContextRelation (CtxBinaryRelation mode m d a) mode m a where
    type RelationDetails (CtxBinaryRelation mode m d a) = d
    relationName   = ctxBinRelName
    assessRelation (CtxBinaryRelation _ rel comb empty) mode inf =
      let assessed = do a <- Set.elems inf
                        b <- Set.elems inf
                        if a == b then []
                                  else maybeToList $ rel mode a b
      in if null assessed then Nothing
                          else Just $ foldr comb empty <$> sequence assessed


instance ContextRelation (CtxWholeRelation mode m d a) mode m a where
  type RelationDetails (CtxWholeRelation mode m d a) = d
  relationName   = ctxWholeRelName
  assessRelation = ctxWholeRel


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Misc

data GenericContext mode dm rm details a = GenericContext{
    _ctxName      :: String
  , _ctxData      :: dm Information
  , _ctxRelations :: CtxRelations mode rm details a
  }


instance Context (GenericContext mode dm rm det a) a where
  type CtxDataM   (GenericContext mode dm rm det a) = dm
  type CtxRelsM   (GenericContext mode dm rm det a) = rm
  type CtxDetails (GenericContext mode dm rm det a) = det
  type CtxMode    (GenericContext mode dm rm det a) = mode
  ctxName = _ctxName
  ctxData = _ctxData
  ctxRelations = _ctxRelations


-----------------------------------------------------------------------------

assessAtCxt :: (Monad (CtxRelsM c)) =>
               Context c a => c -> CtxMode c -> Information
                           -> CtxRelsM c (a, CtxDetails c)
assessAtCxt c mode inf = fmap (ctxCombineRels $ ctxRelations c)
                       . assessRelations mode inf
                       . ctxRelations' $ ctxRelations c


assessRelations mode inf = sequence . mapMaybe (\r -> assessRelation r mode inf)

-----------------------------------------------------------------------------

newtype Id x = Id x
fromId (Id x) = x

instance Functor     Id where fmap f (Id x)       = Id $ f x
instance Applicative Id where pure                = Id
                              (<*>) (Id f) (Id x) = Id $ f x
instance Monad       Id where (>>=) (Id x) f      = f x

-----------------------------------------------------------------------------
