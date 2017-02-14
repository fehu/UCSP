-----------------------------------------------------------------------------
--
-- Module      :  CSP.Coherence.Context.Filtering
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}


module CSP.Coherence.Context.Filtering(

-- * Convertible

  AFilteringContext(..)
, CtxRelationValueConvertable(..)

, CtxRelationsConvert, ctxRelationsConvert
, ConvertedContextRelation, ctxSomeRelationConvert


-- * Contexts

, FilteringContextPure, newPureFilteringContext
, FilteringContextDataIO, newDataIOFilteringContext
, FilteringContextIO, newIOFilteringContext

-- * Misc

, Threshold(..), getThreshold

, module Export

) where

import Data.Typeable (Typeable)

import CSP.Coherence.Context as Export

import qualified Data.Set as Set

import Control.Arrow (first, second, (&&&))

-----------------------------------------------------------------------------

data AFilteringContext ctx a = forall b . ( FilteringContext ctx b
                                          , CtxRelationValueConvertable b a
                                          ) =>
     AFilteringContext ctx


class CtxRelationValueConvertable a b where
  ctxConvertRelationValue  :: a -> b
  ctxConvertRelationValue' :: b -> a

-----------------------------------------------------------------------------

data CtxRelationsConvert mode m d b = forall rels a . ( CtxRelations' rels mode m d a
                                                      , CtxRelationValueConvertable a b
                                                      ) =>
      CtxRelationsConvert rels

ctxRelationsConvert :: (CtxRelationValueConvertable a b, Functor m) =>
                       CtxRelations mode m d a -> CtxRelations mode m d b
ctxRelationsConvert (CtxRelations rels) = CtxRelations $ CtxRelationsConvert rels

instance (Functor m) =>
  CtxRelations' (CtxRelationsConvert mode m d b) mode m d b where
    ctxRelations' (CtxRelationsConvert rels) = map ctxSomeRelationConvert
                                             $ ctxRelations' rels

    ctxCombineRels (CtxRelationsConvert rels) =
      let convert = map $ first ctxConvertRelationValue'
      in first ctxConvertRelationValue . ctxCombineRels rels . convert


ctxSomeRelationConvert :: (CtxRelationValueConvertable a b, Functor m) =>
                          SomeContextRelation mode m a
                       -> SomeContextRelation mode m b
ctxSomeRelationConvert (SomeContextRelation rel) = SomeContextRelation
                                                 $ ConvertedContextRelation rel


data ConvertedContextRelation mode m b = forall rel a . ( ContextRelation rel mode m a
                                                        -- , RelationDetails rel ~ SomeRelationDetails
                                                        , CtxRelationValueConvertable a b
                                                        ) =>
     ConvertedContextRelation rel

instance (Functor m) =>
  ContextRelation (ConvertedContextRelation mode m a) mode m a where
    type RelationDetails (ConvertedContextRelation mode m a) = SomeRelationDetails
    relationName   (ConvertedContextRelation rel) = relationName rel
    assessRelation (ConvertedContextRelation rel) mode =
      fmap (fmap ( first ctxConvertRelationValue
                 . second (SomeRelationDetails rel)
                 )
            ) . assessRelation rel mode


instance (Functor (CtxRelsM ctx)) =>
  Context (AFilteringContext ctx a) a where
    type CtxDataM   (AFilteringContext ctx a) = CtxDataM ctx
    type CtxRelsM   (AFilteringContext ctx a) = CtxRelsM ctx
    type CtxDetails (AFilteringContext ctx a) = CtxDetails ctx
    type CtxMode    (AFilteringContext ctx a) = CtxMode ctx
    ctxName       (AFilteringContext c) = ctxName c
    ctxData       (AFilteringContext c) = ctxData c
    ctxRelations  (AFilteringContext c) = ctxRelationsConvert $ ctxRelations c



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

newtype Threshold a = Threshold a deriving (Show, Eq, Ord, Typeable)
instance (Typeable a, Show a, Ord a) =>
  InformationPiece (Threshold a) where informationType _ = "Threshold"

getThreshold :: Threshold a -> a
getThreshold (Threshold x) = x

-----------------------------------------------------------------------------

type FilteringContextPure mode details a = GenericContext mode Id Id details a
newPureFilteringContext :: (Typeable a, Show a, Ord a) =>
                           String
                        -> Information
                        -> CtxRelations mode Id details a
                        -> Threshold a
                        -> FilteringContextPure mode details a
newPureFilteringContext name inf rels thr = GenericContext name inf' rels
  where inf' = Id $ Set.insert (SomeInformationPiece thr) inf



instance (Typeable a, Show a, Ord a) =>
  FilteringContext (FilteringContextPure mode d a) a where
    type CtxThreM   (FilteringContextPure mode d a) = Id
    type CtxFilterM (FilteringContextPure mode d a) = Id
    ctxThreshold = ctxThreshold'
    isCoherentAtCtx c = isCoherentAtCtx' (fromId $ ctxThreshold c) c
    isCoherentAtCtxIO c mode = return . fromId . isCoherentAtCtx c mode


ctxThreshold' ctx = head . collectInformation getThreshold <$> ctxData ctx

isCoherentAtCtx' thr c mode = fmap (((>= thr) . fst) &&& id) . assessAtCxt c mode

-----------------------------------------------------------------------------

type FilteringContextDataIO mode details a = GenericContext mode IO Id details a
newDataIOFilteringContext :: (Typeable a, Show a, Ord a) =>
                             String
                          -> IO Information
                          -> CtxRelations mode Id details a
                          -> IO (Threshold a)
                          -> FilteringContextDataIO mode details a
newDataIOFilteringContext name inf rels thr =
  GenericContext name (addThreshold inf thr) rels

addThreshold inf thr = Set.insert <$> fmap SomeInformationPiece thr <*> inf

instance (Typeable a, Show a, Ord a) =>
  FilteringContext (FilteringContextDataIO mode details a) a where
    type CtxThreM   (FilteringContextDataIO mode details a) = IO
    type CtxFilterM (FilteringContextDataIO mode details a) = IO
    ctxThreshold = ctxThreshold'
    isCoherentAtCtx c mode inf = do thr <- ctxThreshold c
                                    return . fromId $
                                      isCoherentAtCtx' thr c mode inf
    isCoherentAtCtxIO = isCoherentAtCtx


-----------------------------------------------------------------------------

type FilteringContextIO mode details a = GenericContext mode IO IO details a
newIOFilteringContext :: (Typeable a, Show a, Ord a) =>
                         String
                      -> IO Information
                      -> CtxRelations mode IO details a
                      -> IO (Threshold a)
                      -> FilteringContextIO mode details a
newIOFilteringContext name inf rels thr =
  GenericContext name (addThreshold inf thr) rels

instance (Typeable a, Show a, Ord a) =>
  FilteringContext (FilteringContextIO mode details a) a where
    type CtxThreM   (FilteringContextIO mode details a) = IO
    type CtxFilterM (FilteringContextIO mode details a) = IO
    ctxThreshold = ctxThreshold'
    isCoherentAtCtx c mode inf = do thr <- ctxThreshold c
                                    isCoherentAtCtx' thr c mode inf
    isCoherentAtCtxIO = isCoherentAtCtx


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
