-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Contexts
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------


module AUCSP.Contexts(

  Contexts(..), propagateThroughContexts

, module Export

) where

import AUCSP.Context             as Export
import AUCSP.Context.Obligations as Export
import AUCSP.Context.Preferences as Export
import AUCSP.Context.External    as Export

import CSP.Coherence.Information as Export
import CSP.Coherence.Context     as Export
import CSP.Coherence.Candidate   as Export

import qualified CSP.Coherence as CSP
import CSP.Coherence.Context.Filtering.Convert

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

data Contexts a = Contexts {
    obligations :: ObligationsContext
  , preferences :: PreferencesContext a
  , external    :: ExternalContext a
  }

propagateThroughContexts :: (Num a, Ord a, Show a, Typeable a) =>
                            Contexts a
                         -> ContextMode
                         -> Candidate a details
                         -> IO (Candidate a details)

propagateThroughContexts cxts =
  CSP.propagateThroughContexts $ fmap ($ cxts)
    [ CSP.SomeFilteringContext . AFilteringContext . obligations
    , CSP.SomeFilteringContext . AFilteringContext . preferences
    , CSP.SomeCombiningFilteringContext            . external
    ]

-----------------------------------------------------------------------------
