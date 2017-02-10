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

  Context(..) -- , SomeContext(..)
, CtxRelations(..)

, ContextRelation(..), SomeRelationDetails(..)

, FilteringContext(..), ModifyingContext(..)

-- * Relations

, CtxBinaryRelations(..), ctxBinaryRelations
, CtxBinaryRelation(..) -- , CtxBinaryRelation'(..)
, CtxWholeRelations(..), ctxWholeRelations
, CtxWholeRelation(..) -- , CtxWholeRelation'(..)


-- * Misc

, GenericContext(..), assessAtCxt

, Id(..), fromId

, module CSP.Coherence.Information

) where

import CSP.Coherence.Information

import Data.Maybe (maybeToList, mapMaybe)

import qualified Data.Set as Set

import Control.Arrow (second)

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

    ctxName      :: c -> String
    ctxData      :: c -> CtxDataM c Information
    ctxRelations :: c -> CtxRelations (CtxRelsM c) (CtxDetails c) a
    --

-- data SomeContext a = forall c . Context c a => SomeContext c

data CtxRelations m d a = CtxRelations{
    ctxRelsBinary      :: CtxBinaryRelations m a
  , ctxRelsWhole       :: CtxWholeRelations  m a
  , ctxCombineBinWhole :: [(a, SomeRelationDetails)]
                       -> [(a, SomeRelationDetails)]
                       -> (a, d)
  }

-----------------------------------------------------------------------------

-- | A context could be used for filtering coherent information.
class (Context c a) => FilteringContext c a | c -> a
  where
    type CtxThreM c :: * -> *
    ctxThreshold :: c -> CtxThreM c a

    type CtxFilterM c :: * -> *
    isCoherentAtCtx   :: c -> Information
                      -> CtxFilterM c (Bool, (a, CtxDetails c))
    isCoherentAtCtxIO :: c -> Information
                      -> IO (Bool, (a, CtxDetails c))

-- | A context could be used to modify/create some information.
class (Context c a) => ModifyingContext c a | c -> a
  where
    type CtxResultM c :: * -> *
    processAtCtx   :: c -> Information -> CtxResultM c (Information, CtxDetails c)
    processAtCtxIO :: c -> Information -> IO           (Information, CtxDetails c)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


class ContextRelation rel m a | rel -> m, rel -> a
  where
    type RelationDetails rel :: *
    assessRelation :: rel -> Information -> Maybe (m (a, RelationDetails rel))

data SomeRelationDetails = forall rel m a . ContextRelation rel m a =>
     SomeRelationDetails rel (RelationDetails rel)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- * Relations

data CtxBinaryRelations m a = CtxBinaryRelations [CtxBinaryRelation' m a]
                            | CtxBinaryNoRels

ctxBinaryRelations :: CtxBinaryRelations m a -> [CtxBinaryRelation' m a]
ctxBinaryRelations (CtxBinaryRelations rels) = rels
ctxBinaryRelations _                         = []

data CtxBinaryRelation m d a = forall d' . CtxBinaryRelation {
    ctxBinRel :: SomeInformationPiece
              -> SomeInformationPiece
              -> Maybe (m (a, d'))
  , ctxBinRelsCombine :: (a, d') -> (a, d) -> (a, d)
  , emptyBinaryRelRes :: (a, d)
  }

data CtxBinaryRelation' m a =
    forall d . ContextRelation (CtxBinaryRelation m d a) m a =>
    CtxBinaryRelation' (CtxBinaryRelation m d a)

instance (Functor m) => ContextRelation (CtxBinaryRelation' m a) m a where
  type RelationDetails (CtxBinaryRelation' m a) = SomeRelationDetails
  assessRelation (CtxBinaryRelation' rel) = assessRelation' rel

assessRelation' rel = fmap (fmap (second $ SomeRelationDetails rel))
                    . assessRelation rel

-----------------------------------------------------------------------------


data CtxWholeRelations m a = CtxWholeRelations [CtxWholeRelation' m a]
                           | CtxWholeNoRels

ctxWholeRelations :: CtxWholeRelations m a -> [CtxWholeRelation' m a]
ctxWholeRelations (CtxWholeRelations rels) = rels
ctxWholeRelations _                        = []

data CtxWholeRelation m d a = CtxWholeRelation {
    ctxWholeRel :: Information -> Maybe (m (a, d))
  }

data CtxWholeRelation' m a =
    forall d . ContextRelation (CtxWholeRelation m d a) m a =>
    CtxWholeRelation' (CtxWholeRelation m d a)

instance (Functor m) => ContextRelation (CtxWholeRelation' m a) m a where
  type RelationDetails (CtxWholeRelation' m a) = SomeRelationDetails
  assessRelation (CtxWholeRelation' rel) = assessRelation' rel



-----------------------------------------------------------------------------

instance ContextRelation (CtxWholeRelation m d a) m a where
  type RelationDetails (CtxWholeRelation m d a) = d
  assessRelation (CtxWholeRelation rel) = rel


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Misc

data GenericContext dm rm details a = GenericContext{
    _ctxName      :: String
  , _ctxData      :: dm Information
  , _ctxRelations :: CtxRelations rm details a
  , _ctxDetails   :: [SomeRelationDetails] -> details
  -- , _ctxExtra     :: extra
  }


instance Context (GenericContext dm rm det a) a where
  type CtxDataM  (GenericContext dm rm det a) = dm
  type CtxRelsM  (GenericContext dm rm det a) = rm
  type CtxDetails (GenericContext dm rm det a) = det
  ctxName = _ctxName
  ctxData = _ctxData
  ctxRelations = _ctxRelations


-----------------------------------------------------------------------------

assessAtCxt c inf = do
  let rels = ctxRelations c
  binary <- assessRelations inf . ctxBinaryRelations $ ctxRelsBinary rels
  whole  <- assessRelations inf . ctxWholeRelations  $ ctxRelsWhole rels
  return $ ctxCombineBinWhole rels binary whole


assessRelations inf = sequence . mapMaybe (`assessRelation` inf)

-----------------------------------------------------------------------------

newtype Id x = Id x
fromId (Id x) = x

instance Functor     Id where fmap f (Id x)       = Id $ f x
instance Applicative Id where pure                = Id
                              (<*>) (Id f) (Id x) = Id $ f x
instance Monad       Id where (>>=) (Id x) f      = f x

-----------------------------------------------------------------------------
