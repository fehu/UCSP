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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module CSP.Coherence.Candidate(

  Candidate(..)
, CtxAssessment(..)

) where

import CSP.Coherence.Context

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------


data Candidate a d = Candidate{ candidateInfo       :: Information
                              , candidateCoherent   :: Maybe Bool
                              , candidateCoherence  :: a
                              , candidateDetails    :: d
                              , candidateAssessment :: [CtxAssessment a]
                              }
  deriving Typeable
deriving instance (Show a, Show d) => Show (Candidate a d)

data CtxAssessment a = forall c . (Context c a, Show (CtxDetails c)) =>
     CtxAssessment c a (CtxDetails c)
  deriving Typeable

instance Show a => Show (CtxAssessment a) where
  show (CtxAssessment ctx a d) = "CtxAssessment[" ++ ctxName ctx ++ "]="
                               ++ show a ++ " (" ++ show d ++ ")"

-----------------------------------------------------------------------------

-- assessCandidate
