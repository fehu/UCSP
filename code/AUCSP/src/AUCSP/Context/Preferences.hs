-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Context.Preferences
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AUCSP.Context.Preferences (

  PreferencesContext, newPreferencesContext

, module Export

) where

import CSP.Coherence.Context.Filtering.Convert

import AUCSP.Utils.InInterval as Export
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

inUnitIntervalProduct :: (Num a, Ord a) => [InUnitInterval a] -> InUnitInterval a
inUnitIntervalProduct = toInterval . product . map fromInterval

-----------------------------------------------------------------------------

instance (Num a, Ord a, IntValue min, IntValue max) =>
  CtxRelationValueConvertible (InAnInterval min max a) a where
    ctxConvertRelationValue  = fromInterval
    ctxConvertRelationValue' = toInterval

-----------------------------------------------------------------------------
