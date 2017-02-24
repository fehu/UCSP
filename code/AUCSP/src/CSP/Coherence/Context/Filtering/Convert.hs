-----------------------------------------------------------------------------
--
-- Module      :  CSP.Coherence.Context.Filtering.Convert
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module CSP.Coherence.Context.Filtering.Convert(

  AFilteringContext(..)
, CtxRelationValueConvertible(..)
, FilteringContextConstraints

-- , CtxRelationsConvert, ctxRelationsConvert
-- , ConvertedContextRelation, ctxSomeRelationConvert

) where

import CSP.Coherence.Context

import Control.Arrow (first, second)

-----------------------------------------------------------------------------

data AFilteringContext ctx a = forall b . ( FilteringContext ctx b
                                          , CtxRelationValueConvertible b a
                                          ) =>
     AFilteringContext ctx


class CtxRelationValueConvertible a b where
  ctxConvertRelationValue  :: a -> b
  ctxConvertRelationValue' :: b -> a

-----------------------------------------------------------------------------

data CtxRelationsConvert mode m d b = forall rels a . ( CtxRelations' rels mode m d a
                                                      , CtxRelationValueConvertible a b
                                                      ) =>
      CtxRelationsConvert rels

ctxRelationsConvert :: (CtxRelationValueConvertible a b, Functor m) =>
                       CtxRelations mode m d a -> CtxRelations mode m d b
ctxRelationsConvert (CtxRelations rels) = CtxRelations $ CtxRelationsConvert rels

instance (Functor m) =>
  CtxRelations' (CtxRelationsConvert mode m d b) mode m d b where
    ctxRelations' (CtxRelationsConvert rels) = map ctxSomeRelationConvert
                                             $ ctxRelations' rels

    ctxCombineRels (CtxRelationsConvert rels) =
      let convert = map $ first ctxConvertRelationValue'
      in first ctxConvertRelationValue . ctxCombineRels rels . convert


ctxSomeRelationConvert :: (CtxRelationValueConvertible a b, Functor m) =>
                          SomeContextRelation mode m a
                       -> SomeContextRelation mode m b
ctxSomeRelationConvert (SomeContextRelation rel) = SomeContextRelation
                                                 $ ConvertedContextRelation rel


data ConvertedContextRelation mode m b = forall rel a . ( ContextRelation rel mode m a
                                                        -- , RelationDetails rel ~ SomeRelationDetails
                                                        , CtxRelationValueConvertible a b
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

type FilteringContextConstraints ctx =
  (Functor (CtxRelsM ctx), Functor (CtxThreM ctx), Monad (CtxFilterM ctx))

instance (FilteringContextConstraints ctx) =>
  FilteringContext (AFilteringContext ctx a) a where
    type CtxThreM   (AFilteringContext ctx a) = CtxThreM ctx
    type CtxFilterM (AFilteringContext ctx a) = CtxFilterM ctx

    ctxThreshold (AFilteringContext c) = ctxConvertRelationValue <$> ctxThreshold c
    isCoherentAtCtx (AFilteringContext c) mode =
      fmap (second (first ctxConvertRelationValue)) . isCoherentAtCtx c mode
    isCoherentAtCtxIO (AFilteringContext c) mode =
      fmap (second (first ctxConvertRelationValue)) . isCoherentAtCtxIO c mode

-----------------------------------------------------------------------------
