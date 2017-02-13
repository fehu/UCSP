-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Utils.InUnitInterval
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module AUCSP.Utils.InUnitInterval (

  InUnitInterval, fromUnitInterval, inUnitInterval, toUnitInterval
, inUnitIntervalProduct

) where


-----------------------------------------------------------------------------

newtype InUnitInterval a = InUnitInterval a deriving (Eq, Ord)

fromUnitInterval :: InUnitInterval a -> a
fromUnitInterval (InUnitInterval x) = x

instance Show a => Show (InUnitInterval a) where show = show . fromUnitInterval

inUnitInterval :: (Fractional a, Ord a) => a -> Maybe (InUnitInterval a)
inUnitInterval x | x >= 0 && x <= 1 = Just $ InUnitInterval x
                 | otherwise        = Nothing

toUnitInterval :: (Fractional a, Ord a) => a -> InUnitInterval a
toUnitInterval x | x < 0     = InUnitInterval 0
                 | x > 1     = InUnitInterval 1
                 | otherwise = InUnitInterval x

inUnitIntervalProduct :: (Fractional a, Ord a) =>
                         [InUnitInterval a] -> InUnitInterval a
inUnitIntervalProduct = InUnitInterval . product . map fromUnitInterval

-----------------------------------------------------------------------------
