-----------------------------------------------------------------------------
--
-- Module      :  CSP.Coherence
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module CSP.Coherence(

  -- CoherenceAssessment(..)

  module Export

) where

import CSP.Coherence.Information as Export
import CSP.Coherence.Context     as Export
import CSP.Coherence.Candidate   as Export


-----------------------------------------------------------------------------

-- | Coherence is calculated over some information using the contexts.
--   A context represents some aspect of problem model.

-- class CoherenceAssessment c a | c -> a
--   where
--     type Coherence c :: * -> *
--     coherenceOf :: c -> Information -> Coherence c a



-----------------------------------------------------------------------------
