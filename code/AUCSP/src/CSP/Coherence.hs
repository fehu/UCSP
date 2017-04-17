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
{-# LANGUAGE FlexibleContexts #-}

module CSP.Coherence(

  SomeFilteringContext(..)
, propagateThroughContexts

, module Export

) where

import CSP.Coherence.Information as Export
import CSP.Coherence.Context     as Export
import CSP.Coherence.Candidate   as Export

-----------------------------------------------------------------------------

data SomeFilteringContext mode a =
    forall c . ( FilteringContext c a
               , CtxMode c ~ mode, Show (CtxDetails c)
               ) =>
             SomeFilteringContext c
  | forall c b . ( CombiningFilteringContext c a b
                 , CtxMode c ~ mode, Show (CtxDetails c)
                 ) =>
               SomeCombiningFilteringContext c

-- | Sums coherence values, evaluated at each context,
--   until some value doesn't pass the threshold.
--   After that the candidate is lest as is.
propagateThroughContexts :: (Num a, Ord a) =>
                            [SomeFilteringContext mode a] -> mode
                         -> Candidate a details -> IO (Candidate a details)
propagateThroughContexts [] mode candidate = return candidate
propagateThroughContexts (SomeFilteringContext c : cs) mode candidate =
  do (cohBool, (cohVal, details')) <- isCoherentAtCtxIO c mode
                                    $ candidateInfo candidate
     updateAndPropagate candidate c cs mode cohBool cohVal details'
propagateThroughContexts (SomeCombiningFilteringContext c : cs) mode candidate =
  do (_, (cohB, details')) <- isCoherentAtCtxIO c mode $ candidateInfo candidate
     threshold <- combinedThresholdIO c
     let coherence = combineCoherence c (candidateCoherence candidate) cohB
         coherent  = coherence >= threshold
     updateAndPropagate candidate c cs mode coherent coherence details'



updateAndPropagate candidate c cs mode cohBool cohVal details' =
  let details = CtxAssessment c cohVal details'
      candidate' = candidate {
          candidateCoherent   = Just cohBool
        , candidateCoherence  = cohVal
        , candidateAssessment = details : candidateAssessment candidate
        }
  in if cohBool then propagateThroughContexts cs mode candidate'
                else return candidate'

-----------------------------------------------------------------------------
