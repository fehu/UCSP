-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule.Internal
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module AUCSP.Agent.SharedSchedule.Internal where

import AgentSystem.Generic
import AUCSP.Classes

import Data.Typeable
import Data.Maybe (fromJust)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Concurrent.STM

-----------------------------------------------------------------------------

data ScheduleHolder = ScheduleHolder   deriving (Show, Eq, Ord, Typeable)


-----------------------------------------------------------------------------
-- * States

-- | Table Day-DiscreteTime with variable cells
data Timetable = forall td . DiscreteTimeDescriptor td =>
     Timetable (Map (Day, DTime td) TimeSlot)

newtype TimeSlot = TimeSlot (TVar (Maybe ClassCore))


-----------------------------------------------------------------------------
-- * Messages

data ScheduleHolderReset = ScheduleHolderReset deriving (Typeable, Show)


-----------------------------------------------------------------------------
-- * Implementation: ScheduleHolder

instance RoleName ScheduleHolder where roleName = show
instance AgentRole ScheduleHolder where
  type RoleResult ScheduleHolder = ()
  type RoleState  ScheduleHolder = (Timetable, Classroom)
  type RoleArgs   ScheduleHolder = (SomeDiscreteTimeDescriptor, Classroom)

scheduleHolderDescriptor :: Bool -> GenericRoleDescriptor ScheduleHolder
scheduleHolderDescriptor debug =
  genericRoleDescriptor ScheduleHolder $
    \(td, room) -> return GenericAgentDescriptor{
        agName = "ScheduleHolder[" ++ show room ++ "]"
      , agDebug = debug
      , initialState = (,) <$> newTimetable td <*> return room
      , messageHandling = MessageHandling{
            msgHandle = selectMessageHandler [
              mbHandle $ \i ScheduleHolderReset -> resetTimetable $ agentState i
            ]
          , msgRespond = selectResponse []
          }
      , action = agentNoAction
      , emptyResult = EmptyResult
      }

newTimetable :: SomeDiscreteTimeDescriptor -> IO Timetable
newTimetable td = fmap (Timetable . Map.fromList) . sequence $
                  do day  <- [minBound .. maxBound]
                     time <- [dTimeMin td, dTimeMin td + dTimeStep td
                              .. dTimeMax td]
                     let mkTime = fromJust . dTime td
                         newTimeSlot = TimeSlot <$> newTVarIO Nothing
                     return $ (,) <$> return (day, mkTime time) <*> newTimeSlot


resetTimetable :: (Timetable, Classroom) -> IO ()
resetTimetable (Timetable tt, _) = atomically . mapM_ resetTimeSlot $ Map.elems tt
  where resetTimeSlot (TimeSlot v) = writeTVar v Nothing


-----------------------------------------------------------------------------
