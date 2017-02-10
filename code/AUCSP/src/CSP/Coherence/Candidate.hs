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
, CandidateAssessment(..)

) where

import CSP.Coherence.Context

-----------------------------------------------------------------------------


data Candidate a d = Candidate{ candidateInfo       :: Information
                              , candidateCoherent   :: Maybe Bool
                              , candidateCoherence  :: a
                              , candidateDetails    :: d
                              , candidateAssessment :: [CandidateAssessment a]
                              }

data CandidateAssessment a = forall c . Context c a =>
    CandidateAssessment c a (CtxDetails c)


-----------------------------------------------------------------------------

-- assessCandidate
