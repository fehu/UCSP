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
{-# LANGUAGE FlexibleContexts #-}


module CSP.Coherence.Context.Filtering(

-- * Contexts

  FilteringContextPure, newPureFilteringContext
, FilteringContextDataIO, newDataIOFilteringContext
, FilteringContextIO, newIOFilteringContext

-- * Misc

, Threshold(..), getThreshold

, module Export

) where

import Data.Typeable (Typeable)

import CSP.Coherence.Context as Export

import qualified Data.Set as Set

import Control.Arrow ( (&&&) )

-----------------------------------------------------------------------------

newtype Threshold a = Threshold a deriving (Show, Eq, Ord, Typeable)

instance (Typeable a, Show a, Ord a) =>
  InformationPiece (Threshold a) where informationType _ = "Threshold"

getThreshold :: Threshold a -> a
getThreshold (Threshold x) = x

instance Functor Threshold where fmap f = Threshold . f . getThreshold

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
