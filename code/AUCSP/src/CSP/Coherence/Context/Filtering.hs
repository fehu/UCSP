-----------------------------------------------------------------------------
--
-- Module      :  CSP.Coherence.Context.Filtering
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------


module CSP.Coherence.Context.Filtering(

-- * Pure

  FilteringContextPure(..), newPureFilteringContext

) where

import CSP.Coherence.Context
import Control.Arrow ( (&&&) )

-----------------------------------------------------------------------------


data CtxFilteringExtra tm a = CtxFilteringExtra{
    _ctxThreshold :: tm a
  }


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type FilteringContextPure details a = GenericContext Id Id details (CtxFilteringExtra Id a) a
newPureFilteringContext :: String
                        -> Information
                        -> CtxRelations Id details a
                        -> a
                        -> ([SomeRelationDetails] -> details)
                        -> FilteringContextPure details a
newPureFilteringContext name inf rels thr df = GenericContext name (Id inf) rels df
                                             $ CtxFilteringExtra (Id thr)

instance (Ord a) => FilteringContext (FilteringContextPure d a) a where
  type CtxThreM   (FilteringContextPure d a) = Id
  type CtxFilterM (FilteringContextPure d a) = Id
  ctxThreshold = _ctxThreshold . _ctxExtra
  isCoherentAtCtx c = fmap (((>= thr) . fst) &&& id) . assessAtCxt c
    where thr = fromId $ ctxThreshold c
  isCoherentAtCtxIO c = return . fromId . isCoherentAtCtx c




-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
