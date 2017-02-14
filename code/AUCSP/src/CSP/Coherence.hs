-----------------------------------------------------------------------------
--
-- Module      :  CSP.Coherence
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module CSP.Coherence(

  SomeFilteringContext(..)
, AssessmentDetails, CtxAssessment(..)
, propagateThroughContexts

, module Export

) where

import CSP.Coherence.Information as Export
import CSP.Coherence.Context     as Export
import CSP.Coherence.Candidate   as Export

import Control.Arrow (first, second)

-----------------------------------------------------------------------------

data SomeFilteringContext mode a = forall c . ( FilteringContext c a
                                              , CtxMode c ~ mode
                                              ) =>
     SomeContext c

data CtxAssessment a = forall c . FilteringContext c a =>
     CtxAssessment c Bool a (CtxDetails c)

type AssessmentDetails a = [CtxAssessment a]

-- | Sums coherence.
propagateThroughContexts :: (Num a) =>
                            [SomeFilteringContext mode a] -> mode -> Information
                         -> IO (a, AssessmentDetails a)
propagateThroughContexts (SomeContext c : cs) mode inf =
  do (cohBool, (cohVal, details')) <- isCoherentAtCtxIO c mode inf
     let details = CtxAssessment c cohBool cohVal details'
     (first (cohVal +) . second (details :))
      <$> propagateThroughContexts cs mode inf

-----------------------------------------------------------------------------
