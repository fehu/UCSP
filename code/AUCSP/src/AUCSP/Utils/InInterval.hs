-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Utils.InInterval
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Utils.InInterval (

  InInterval(..)

, Int' (Pos, Zero, Neg), IntValue(intValue)

, InAnInterval, scaleToInterval
, InUnitInterval


, RestrictedFunctor(..)
, InIntervalOps(..)
-- , InIntervalCombine(..)

, Proxy(..)

) where

import Data.Proxy
import Data.Type.Bool
import Data.Function (on)

import GHC.TypeLits

-----------------------------------------------------------------------------

data Int' = Pos Nat
          | Zero
          | Neg Nat

class IntValue (i :: Int') where intValue :: Proxy i -> Integer
instance (KnownNat n) => IntValue (Pos n)
  where intValue = let getNatProxy :: Proxy (Pos n) -> Proxy n
                       getNatProxy _ = Proxy
                   in natVal . getNatProxy

instance IntValue Zero where intValue = const 0

instance (KnownNat n) => IntValue (Neg n)
  where intValue = let getNatProxy :: Proxy (Neg n) -> Proxy n
                       getNatProxy _ = Proxy
                   in negate . natVal . getNatProxy



-----------------------------------------------------------------------------

class (Num a, Ord a) => InInterval i a | i -> a
  where
    type IntervalMin i :: Int'
    type IntervalMax i :: Int'

    intervalMin :: i -> a
    intervalMax :: i -> a

    fromInterval :: i -> a

    inInterval :: a -> Maybe i
    toInterval :: a -> i



-- -- | move: left -> new left
-- --   scale: right-left -> new right - new left
-- scaleToInterval :: ( Fractional a, Ord a
--                    , InInterval i a
--                    , InInterval i' a
--                    , IntervalMin i' ~ Fst interval
--                    , IntervalMax i' ~ Snd interval
--                    , IntValue (Fst interval)
--                    , IntValue (Snd interval)
--                     ) =>
--                 i -> Proxy (interval :: (Int', Int')) -> i'
--
-- scaleToInterval i proxy = let newMin = fromInteger . intValue $ proxyFst proxy
--                               newMax = fromInteger . intValue $ proxySnd proxy
--                               shift  = newMin - intervalMin i
--                               scale  = (newMax - newMin)
--                                      / (intervalMax i - intervalMin i)
--                               v = fromInterval i
--                           in toInterval $ (v+shift)*scale

-----------------------------------------------------------------------------

type family Fst (p :: (a, b)) :: a where Fst '(a, b) = a
type family Snd (p :: (a, b)) :: b where Snd '(a, b) = b

proxyFst :: Proxy p -> Proxy (Fst p)
proxyFst = const Proxy

proxySnd :: Proxy p -> Proxy (Snd p)
proxySnd = const Proxy

type family Max (a :: Int') (b :: Int') :: Int'
  where
    Max (Pos x) (Pos y) = Pos (MaxNat x y)
    Max (Pos x) _       = Pos x
    Max Zero (Pos y)    = Pos y
    Max Zero _          = Zero
    Max (Neg x) (Neg y) = Neg (MinNat x y)
    Max (Neg x) y       = y

type family Min (a :: Int') (b :: Int') :: Int'
  where
    Min (Neg x) (Neg y) = Neg (MaxNat x y)
    Min (Neg x) y       = Neg x
    Min Zero (Neg y)    = Neg y
    Min Zero _          = Zero
    Min (Pos x) (Pos y) = Pos (MinNat x y)
    Min (Pos x) y       = y


type family MaxNat (x :: Nat) (y :: Nat) :: Nat
  where MaxNat x y = If (x <=? y) y x

type family MinNat (x :: Nat) (y :: Nat) :: Nat
  where MinNat x y = If (x <=? y) x y

-----------------------------------------------------------------------------

class RestrictedFunctor f a b
  where rmap :: (a -> b) -> f a -> Maybe (f b)

class (InInterval i a) => InIntervalOps i a | i -> a
  where
    inIntervalOp :: (a -> a -> a) -> i -> i -> Maybe i

-- class (InInterval i1 a, InInterval i2 a) => InIntervalCombine i1 i2 a
--   where
--     inIntervalCombine :: ( InInterval i' a
--                          , IntervalMin i' ~ Min (IntervalMin i1) (IntervalMin i2)
--                          , IntervalMax i' ~ Max (IntervalMax i1) (IntervalMax i2)
--                           ) =>
--                       (a -> a -> a) -> i1 -> i2 -> i'

-----------------------------------------------------------------------------

data InAnInterval (min :: Int') (max :: Int') a = InAnInterval a

instance (Num a, Ord a, IntValue min, IntValue max) =>
  Eq (InAnInterval min max a) where (==) = (==) `on` fromInterval

instance (Num a, Ord a, IntValue min, IntValue max) =>
  Ord (InAnInterval min max a) where compare = compare `on` fromInterval

instance (Show a, Num a, Ord a, IntValue min, IntValue max) =>
  Show (InAnInterval min max a) where show = show . fromInterval


instance (Num a, Ord a, IntValue min, IntValue max) =>
  InInterval (InAnInterval min max a) a where
    type IntervalMin (InAnInterval min max a) = min
    type IntervalMax (InAnInterval min max a) = max
    intervalMin = fromInteger . intValue . intervalMinProxy
    intervalMax = fromInteger . intValue . intervalMaxProxy

    fromInterval (InAnInterval x) = x
    inInterval x | x >= min && x <= max = Just i
                 | otherwise            = Nothing
        where (i, min, max) = iMinMax x

    toInterval x | x < min   = InAnInterval min
                 | x > max   = InAnInterval max
                 | otherwise = i
        where (i, min, max) = iMinMax x


intervalMinProxy :: i -> Proxy (IntervalMin i)
intervalMinProxy _ = Proxy

intervalMaxProxy :: i -> Proxy (IntervalMax i)
intervalMaxProxy _ = Proxy

iMinMax x = let i   = InAnInterval x
                min = intervalMin i
                max = intervalMax i
            in (i, min, max)


instance (Num b, Ord b, IntValue min, IntValue max) =>
  RestrictedFunctor (InAnInterval min max) a b where
    rmap f (InAnInterval v) = inInterval $ f v

instance (Num a, Ord a, IntValue min, IntValue max) =>
  InIntervalOps (InAnInterval min max a) a where
    inIntervalOp f = (inInterval .) . (f `on` fromInterval)

-- instance ( Num a, Ord a
--          , IntValue min1, IntValue max1
--          , IntValue min2, IntValue max2
--           ) =>
--   InIntervalCombine (InAnInterval min1 max1 a)
--                     (InAnInterval min2 max2 a) a where
--     inIntervalCombine f x y = toInterval $ f (fromInterval x) (fromInterval y)


-- | move: left -> new left
--   scale: right-left -> new right - new left
scaleToInterval :: ( Fractional a, Ord a
                   , IntValue min, IntValue max
                   , IntValue (Fst interval), IntValue (Snd interval)
                    ) =>
                InAnInterval min max a -> Proxy (interval :: (Int', Int'))
                           -> InAnInterval (Fst interval) (Snd interval) a

scaleToInterval i proxy = let newMin = fromInteger . intValue $ proxyFst proxy
                              newMax = fromInteger . intValue $ proxySnd proxy
                              shift  = newMin - intervalMin i
                              scale  = (newMax - newMin)
                                     / (intervalMax i - intervalMin i)
                              v = fromInterval i
                          in toInterval $ (v+shift)*scale

-----------------------------------------------------------------------------

type InUnitInterval a = InAnInterval Zero (Pos 1) a

-----------------------------------------------------------------------------
