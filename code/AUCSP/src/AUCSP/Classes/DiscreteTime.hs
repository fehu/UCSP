-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Classes.DiscreteTime
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module AUCSP.Classes.DiscreteTime (

  DiscreteTime, dTimeMinutes, dTime
, DiscreteTimeDescriptor(..)
, Minutes

, dTimeAddMinutes

, DiscreteTimeRange, dTimeRange
, dTimeRangeIntersect, dTimeIntersect

, SomeDiscreteTime(..)

) where

import Data.Int (Int8)
import Data.Function (on)

import Text.Printf (printf)

-----------------------------------------------------------------------------

type Minutes = Integer

class DiscreteTimeDescriptor td where  dTimeDescriptorInstance :: td
                                       dTimeId   :: td -> String
                                       dTimeMin  :: td -> Minutes
                                       dTimeMax  :: td -> Minutes
                                       dTimeStep :: td -> Minutes

newtype (DiscreteTimeDescriptor td) =>
  DiscreteTime td = DiscreteTime Int8
  deriving (Eq, Ord)

dTimeDescriptor :: (DiscreteTimeDescriptor td) => DiscreteTime td -> td
dTimeDescriptor _ = dTimeDescriptorInstance

dTimeMinutes :: (DiscreteTimeDescriptor td) => DiscreteTime td -> Minutes
dTimeMinutes t@(DiscreteTime i) = let d = dTimeDescriptor t
                                  in dTimeMin d + toInteger i * dTimeStep d

dTime :: (DiscreteTimeDescriptor td) => Minutes -> Maybe (DiscreteTime td)
dTime m | m < dTimeMin d = Nothing
        | m > dTimeMax d = Nothing
        | r /= 0         = Nothing
        | otherwise      = Just . dTime' d $ fromInteger q
  where d      = dTimeDescriptorInstance
        (q, r) = quotRem (m - dTimeMin d) (dTimeStep d)


dTime' :: td -> Int8 -> DiscreteTime td
dTime' _ = DiscreteTime

-----------------------------------------------------------------------------

dTimePretty t = let (hours, minutes) = dTimeMinutes t `quotRem` 60
                in printf "%2d:%02d" hours minutes

instance (DiscreteTimeDescriptor td) =>
  Show (DiscreteTime td) where show = dTimePretty

instance (DiscreteTimeDescriptor td) =>
  Bounded (DiscreteTime td) where
    minBound = DiscreteTime 1
    maxBound = let d = dTimeDescriptorInstance
               in dTime' d . fromInteger $ dTimeMaxInd d


dTimeMaxInd d = (dTimeMax d - dTimeMin d) `quot` dTimeStep d

instance (DiscreteTimeDescriptor td) =>
  Enum (DiscreteTime td) where
    fromEnum (DiscreteTime i) = fromInteger $ toInteger i
    toEnum i | i < 1     = err
             | i > maxI  = err
             | otherwise = dTime' d . fromInteger $ toInteger i
      where d    = dTimeDescriptorInstance
            maxI = fromInteger $ toInteger i
            err  = error $ "DiscreteTime Enum out of bounds: " ++ show i

-----------------------------------------------------------------------------

data SomeDiscreteTime = forall td . DiscreteTimeDescriptor td =>
    SomeDiscreteTime (DiscreteTime td) td

instance Show SomeDiscreteTime where show (SomeDiscreteTime t _) = show t
instance Eq SomeDiscreteTime where
  (==) (SomeDiscreteTime t1@(DiscreteTime i1) td1)
       (SomeDiscreteTime t2@(DiscreteTime i2) td2) =
    if sameTD then i1 == i2
              else dTimeMinutes t1 == dTimeMinutes t2
    where sameTD = dTimeId td1 == dTimeId td2

instance Ord SomeDiscreteTime where
  compare (SomeDiscreteTime t1@(DiscreteTime i1) td1)
          (SomeDiscreteTime t2@(DiscreteTime i2) td2) =
    if sameTD then i1 `compare` i2
              else dTimeMinutes t1 `compare` dTimeMinutes t2
    where sameTD = dTimeId td1 == dTimeId td2

-----------------------------------------------------------------------------

dTimeAddMinutes :: (DiscreteTimeDescriptor td) =>
                   DiscreteTime td -> Minutes -> Maybe (DiscreteTime td)
dTimeAddMinutes dtime mins = dTime $ dTimeMinutes dtime + mins

newtype DiscreteTimeRange td = DiscreteTimeRange (DiscreteTime td, DiscreteTime td)
dTimeRange :: DiscreteTime td -> DiscreteTime td -> Maybe (DiscreteTimeRange td)
dTimeRange t1 t2 = if t1 <= t2 then Just $ DiscreteTimeRange (t1, t2)
                               else Nothing

dTimeRangeIntersect :: DiscreteTimeRange td -> DiscreteTimeRange td -> Bool
dTimeRangeIntersect (DiscreteTimeRange (x1,x2)) (DiscreteTimeRange (y1,y2)) =
  (x1 `between` (y1,y2)) || (x2 `between` (y1,y2))
  where between v (vmin, vmax) = vmin <= v && v <= vmax

dTimeIntersect :: (DiscreteTime td, DiscreteTime td)
               -> (DiscreteTime td, DiscreteTime td)
               -> Maybe Bool
dTimeIntersect p1 p2 = do r1 <- uncurry dTimeRange p1
                          r2 <- uncurry dTimeRange p2
                          return $ dTimeRangeIntersect r1 r2

-----------------------------------------------------------------------------
