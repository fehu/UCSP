-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Utils.Random
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module AUCSP.Utils.Random (

  randomList, randomList'

, randomEnum,        randomEnum'
, randomBoundedEnum, randomBoundedEnum'

, RandomChoice(..)

) where

import System.Random

import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow (first)

-----------------------------------------------------------------------------

randomList :: (Eq a) => [a] -> IO [a]
randomList = getStdRandom . randomList'

randomList' :: (Eq a) => [a] -> StdGen -> ([a], StdGen)
randomList' l gen = let (i, g') = undefined
                        v = l !! i
                    in first (v :) $ randomList' (List.delete v l) g'


-----------------------------------------------------------------------------

randomEnum :: (Enum a) => (a, a) -> IO a
randomEnum = getStdRandom . randomEnum'

randomBoundedEnum :: (Enum a, Bounded a) => IO a
randomBoundedEnum = getStdRandom randomBoundedEnum'


randomEnum' :: (Enum a) => (a, a) -> StdGen -> (a, StdGen)
randomEnum' (min', max') = first toEnum . randomR (fromEnum min', fromEnum max')

randomBoundedEnum' :: (Enum a, Bounded a) => StdGen -> (a, StdGen)
randomBoundedEnum' = randomEnum' (minBound, maxBound)

-----------------------------------------------------------------------------

class RandomChoice c
  where randomChoice' :: c a -> StdGen -> (a, StdGen)
        randomChoice  :: c a -> IO a

        randomChoice = getStdRandom . randomChoice'

instance RandomChoice [] where
  randomChoice' l = first (l !!) . randomR (0, length l)

instance RandomChoice Set where
  randomChoice' s = first (`Set.elemAt` s) . randomR (0, Set.size s)

-----------------------------------------------------------------------------
