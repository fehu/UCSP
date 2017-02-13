-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Context.Preferences
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------


module AUCSP.Context.Preferences (

  PreferencesContext, newPreferencesContext

, module Export

) where

import AUCSP.Utils.InUnitInterval as Export
import AUCSP.Context

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

type PreferencesContext a = FilteringContextDataIO ContextMode
                                                   DetailsByRel
                                                  (InUnitInterval a)

-- | InUnitInterval relations are combined using `(*)`.
newPreferencesContext :: (Fractional a, Ord a, Show a, Typeable a) =>
                         IO Information -> SomeRelations Id (InUnitInterval a)
                                        -> IO (Threshold (InUnitInterval a))
                                        -> PreferencesContext a
newPreferencesContext cdata rels = newDataIOFilteringContext "Preferences"
                                   cdata (newCtxRels rels inUnitIntervalProduct)

-----------------------------------------------------------------------------
