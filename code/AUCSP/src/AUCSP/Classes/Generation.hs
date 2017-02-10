-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Classes.Generation
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Classes.Generation (

  DayRoomTimeGenerator(..)
, shortUsageDayRoomTimeGenerator

, module Export

) where

import AUCSP.Classes                  as Export
import AUCSP.Classes.CoresGeneration  as Export
import AUCSP.AgentsInterface.RoleData
import AUCSP.Utils.Random

import Data.IORef
import Data.Maybe (fromMaybe)

import Data.Set(Set)
import qualified Data.Set as Set

import Control.Monad (forM)

-----------------------------------------------------------------------------


data DayRoomTimeGenerator td = DayRoomTimeGenerator {
    randomDRT   :: Discipline -> IO (Day, Classroom, DiscreteTime td, DiscreteTime td)
  , resetDRTGen :: IO ()
  }

generateClassDRT :: (DiscreteTimeDescriptor td) =>
                    DayRoomTimeGenerator td -> ClassCore -> IO Class
generateClassDRT tgen core = do
  (day, room, btime, etime) <- tgen `randomDRT` classDiscipline' core
  let mkTime = flip SomeDiscreteTime dTimeDescriptorInstance
  return $ Class core room day (mkTime btime) (mkTime etime)

-- | Guards generated values to avoid repetitions.
--   Don't use same instance without resetting too much.
shortUsageDayRoomTimeGenerator :: (DiscreteTimeDescriptor td) =>
                            Set Classroom -> td -> IO (DayRoomTimeGenerator td)
shortUsageDayRoomTimeGenerator rooms td = do
  historyVar <- newIORef Set.empty

  let randomDRT d = do day  <- randomBoundedEnum
                       room <- randomChoice rooms
                       time <- randomBoundedEnum

                       hist <- readIORef historyVar

                       let lenMinutes = disciplineMinutesPerWeek d
                           endTime    = time `dTimeAddMinutes` toInteger lenMinutes
                           intersect etime (d, r, tb, te) =
                             d == day && r == room &&
                             fromMaybe True (dTimeIntersect (tb,te) (time,etime))
                           ensureNotIntersects etime =
                             if Set.null $ Set.filter (intersect etime) hist
                                then let res = (day, room, time, etime)
                                     in  historyVar `writeIORef` Set.insert res hist
                                      >> return res
                                else randomDRT d
                       maybe (randomDRT d) ensureNotIntersects endTime
      resetGenerator = historyVar `writeIORef` Set.empty

  return $ DayRoomTimeGenerator randomDRT resetGenerator



-----------------------------------------------------------------------------

class ClassesGenerator gen where
  usingNextClassCore :: gen -> IO (Maybe (Set Class))
  usingSameClassCore :: gen -> IO (Maybe (Set Class))

data GenericClassesGenerator r td = GenericClassesGenerator {
    gcgSelfData     :: RoleData r
  , gcgCoresPool    :: IORef GroupClassCorePool
  , gcgCurrentCore  :: IORef (Maybe GroupClassCore)
  , gcgDRTGenerator :: IORef (DayRoomTimeGenerator td)
  }

generateFromGroupCore :: (KnownAgentsConstraints, DiscreteTimeDescriptor td) =>
                         GroupClassCore -> DayRoomTimeGenerator td
                      -> RoleData Group -> IO (Set Class)

generateFromGroupCore core tgen gdata =
  let coreClasses = groupCoreClasses gdata core
  in Set.fromList <$> mapM (generateClassDRT tgen) coreClasses

instance (KnownAgentsConstraints, DiscreteTimeDescriptor td) =>
  ClassesGenerator (GenericClassesGenerator Group td) where
    usingSameClassCore gen = do
      mbCore <- readIORef $ gcgCurrentCore gen
      tgen   <- readIORef $ gcgDRTGenerator gen

      forM mbCore $ \core -> do c <- generateFromGroupCore core tgen $
                                     gcgSelfData gen
                                resetDRTGen tgen
                                return c

    usingNextClassCore gen = do
      pool <- readIORef $ gcgCoresPool gen
      let mbNext = nextClassCore pool
      gcgCurrentCore gen `writeIORef` fmap fst mbNext
      mapM_ (writeIORef (gcgCoresPool gen)) $ fmap snd mbNext
      usingSameClassCore gen



-----------------------------------------------------------------------------
