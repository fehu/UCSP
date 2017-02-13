-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Classes.DiscreteTime
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module AUCSP.Classes.DiscreteTime (

  Minutes, UnderlyingMinutes(..)
, DiscreteTime(..), DiscreteTimeDescriptor(..)

, DTime, dTimeIntersect
, SomeDiscreteTime(..)

, DTimeRange, dTimeRange
, dTimeRangeIntersect, dTimeRangeLength

) where

import Data.Int (Int8)
import Data.Function (on)

import Text.Printf (printf)

-----------------------------------------------------------------------------

type Minutes = Integer

class UnderlyingMinutes t where
  getMinutes  :: t  -> Minutes
  addMinutes  :: t  -> Minutes -> Maybe t
  diffMinutes :: t  -> t       -> Minutes

class (UnderlyingMinutes t, Ord t, DiscreteTimeDescriptor td) =>
  DiscreteTime t td | t -> td where
    dTime           :: td -> Minutes -> Maybe t

    dTimeDescriptor :: t  -> td
    dTimeDescriptor = const dTimeDescriptorInstance

class DiscreteTimeDescriptor td where
  dTimeDescriptorInstance :: td
  dTimeId   :: td -> String
  dTimeMin  :: td -> Minutes
  dTimeMax  :: td -> Minutes
  dTimeStep :: td -> Minutes

-----------------------------------------------------------------------------

data DTime td = DTime Int8 deriving (Eq, Ord)


dTimeDescriptor' :: (DiscreteTimeDescriptor td) => DTime td -> td
dTimeDescriptor' _ = dTimeDescriptorInstance

dTime' :: td -> Int8 -> DTime td
dTime' _ = DTime

newDTime :: (DiscreteTimeDescriptor td) => Minutes -> Maybe (DTime td)
newDTime m | m < dTimeMin d = Nothing
           | m > dTimeMax d = Nothing
           | r /= 0         = Nothing
           | otherwise      = Just . dTime' d $ fromInteger q
  where d      = dTimeDescriptorInstance
        (q, r) = quotRem (m - dTimeMin d) (dTimeStep d)

instance (DiscreteTimeDescriptor td) => UnderlyingMinutes (DTime td) where
  getMinutes t@(DTime i) = let d = dTimeDescriptor' t
                           in dTimeMin d + toInteger i * dTimeStep d
  addMinutes dtime mins  = newDTime $ getMinutes dtime + mins
  diffMinutes begin end = dTimeRangeLength $ DTimeRange begin end

instance (DiscreteTimeDescriptor td) => DiscreteTime (DTime td) td where dTime _ = newDTime


dTimePretty t = let (hours, minutes) = getMinutes t `quotRem` 60
                in printf "%2d:%02d" hours minutes

instance (DiscreteTimeDescriptor td) => Show (DTime td) where show = dTimePretty

instance (DiscreteTimeDescriptor td) => Enum (DTime td) where
  fromEnum (DTime i) = fromInteger $ toInteger i
  toEnum i | i < 1     = err
           | i > maxI  = err
           | otherwise = dTime' d . fromInteger $ toInteger i
    where d    = dTimeDescriptorInstance
          maxI = fromInteger $ toInteger i
          err  = error $ "DiscreteTime Enum out of bounds: " ++ show i

instance (DiscreteTimeDescriptor td) =>
  Bounded (DTime td) where
    minBound = DTime 1
    maxBound = let d = dTimeDescriptorInstance
               in dTime' d . fromInteger $ dTimeMaxInd d


dTimeMaxInd d = (dTimeMax d - dTimeMin d) `quot` dTimeStep d


dTimeIntersect :: (DTime td, DTime td)
               -> (DTime td, DTime td)
               -> Maybe Bool
dTimeIntersect p1 p2 = do r1 <- uncurry dTimeRange p1
                          r2 <- uncurry dTimeRange p2
                          return $ dTimeRangeIntersect r1 r2

-----------------------------------------------------------------------------

data DTimeRange td = DTimeRange (DTime td) (DTime td)
  deriving (Show, Eq, Ord)

dTimeRange :: DTime td -> DTime td -> Maybe (DTimeRange td)
dTimeRange t1 t2 = if t1 <= t2 then Just $ DTimeRange t1 t2 else Nothing

dTimeRangeIntersect :: DTimeRange td -> DTimeRange td -> Bool
dTimeRangeIntersect (DTimeRange x1 x2) (DTimeRange y1 y2) =
  (x1 `between` (y1,y2)) || (x2 `between` (y1,y2))
  where between v (vmin, vmax) = vmin <= v && v <= vmax

dTimeRangeLength :: (DiscreteTimeDescriptor td) => DTimeRange td -> Minutes
dTimeRangeLength (DTimeRange t@(DTime b) (DTime e)) =
  dTimeStep (dTimeDescriptor t) * toInteger (e - b)

-----------------------------------------------------------------------------

data SomeDiscreteTime = forall t td . (DiscreteTime t td, Show t) =>
     SomeDiscreteTime t

instance UnderlyingMinutes SomeDiscreteTime where
  getMinutes  (SomeDiscreteTime t) = getMinutes t
  addMinutes  (SomeDiscreteTime t) = fmap SomeDiscreteTime . addMinutes t
  diffMinutes e b = getMinutes e - getMinutes b

instance Eq   SomeDiscreteTime where (==) = (==) `on` getMinutes
instance Ord  SomeDiscreteTime where compare = compare `on` getMinutes
instance Show SomeDiscreteTime where show (SomeDiscreteTime t) = show t

-----------------------------------------------------------------------------
