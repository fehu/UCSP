-----------------------------------------------------------------------------
--
-- Module      :  CSP.Coherence.Candidate
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module CSP.Coherence.Candidate(

  Candidate(..)
, CtxAssessment(..)

) where

import CSP.Coherence.Context

-----------------------------------------------------------------------------


data Candidate a d = Candidate{ candidateInfo       :: Information
                              , candidateCoherent   :: Maybe Bool
                              , candidateCoherence  :: a
                              , candidateDetails    :: d
                              , candidateAssessment :: [CtxAssessment a]
                              }

data CtxAssessment a = forall c . FilteringContext c a =>
     CtxAssessment c a (CtxDetails c)


-----------------------------------------------------------------------------

-- assessCandidate
