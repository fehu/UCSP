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
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.SharedSchedule.Internal where

import AgentSystem.Generic
import AUCSP.Classes
import AUCSP.Agent.SharedSchedule.Interface

import Data.Typeable
import Data.Maybe (fromJust, isJust)

import Control.Monad (forM)

import Data.Map.Strict (Map)
import Data.Set        (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Control.Concurrent.STM

-----------------------------------------------------------------------------

data ScheduleHolder = ScheduleHolder deriving (Show, Eq, Ord, Typeable)


-----------------------------------------------------------------------------
-- * States

-- | Table Day-DiscreteTime with variable cells
data Timetable = forall td . DiscreteTimeDescriptor td =>
     Timetable (Map (Day, DTime td) TimeSlot)

newtype TimeSlot = TimeSlot { timeSlotVar :: TVar (Maybe Class) }


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

scheduleHolderDescriptor :: NegotiatorsConstraint =>
                            Bool -> GenericRoleDescriptor ScheduleHolder
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
          , msgRespond = selectResponse [
              mbResp $ \i (TryPutClasses cs) -> scheduleHolderTryPutClasses
                                                (agentState i) cs
            ]
          }
      , action = agentNoAction
      , emptyResult = EmptyResult
      }

timeRange :: DiscreteTime time td => td -> [time] -> [time]
timeRange _ = id

newTimetable :: SomeDiscreteTimeDescriptor -> IO Timetable
newTimetable (SomeDiscreteTimeDescriptor td) =
  fmap (Timetable . Map.fromList) . sequence $
  do day  <- [minBound .. maxBound]
     time <- timeRange td [minBound .. maxBound]
     let newTimeSlot = TimeSlot <$> newTVarIO Nothing
     return $ (,) <$> return (day, time) <*> newTimeSlot


resetTimetable :: (Timetable, Classroom) -> IO ()
resetTimetable (Timetable tt, _) = atomically . mapM_ resetTimeSlot $ Map.elems tt
  where resetTimeSlot = (`writeTVar` Nothing) . timeSlotVar


scheduleHolderTryPutClasses :: NegotiatorsConstraint =>
                    (Timetable, Classroom) -> Set Class
                 -> IO (MsgResponse (AwaitingConfirmation PutClassesResult))
scheduleHolderTryPutClasses (tt, room) cs = fmap combinePutClassesResult
                                          . mapM (putClassInTimetable tt)
                                          . filter ((==) room . classRoom)
                                          $ Set.toList cs
  where combinePutClassesResult :: [(PutClassesResult, IO ())]
                                -> MsgResponse (AwaitingConfirmation PutClassesResult)
        combinePutClassesResult xs =
          let conflicts = Set.unions $ map (putClassesConflicts . fst) xs
              res = if Set.null conflicts
                      then PutClassesSuccess
                      else PutClassesConflict conflicts
          in respondAwaitingConfirmation res (mapM_ snd xs) (return ())

-- | 1. find timeslots for the class
--   2. If any is occupied, return conflicting classes, otherwise write class.
putClassInTimetable :: NegotiatorsConstraint =>
                       Timetable -> Class
                    -> IO (PutClassesResult, IO ())
putClassInTimetable (Timetable cmap) c = do
  let td' :: (DiscreteTimeDescriptor td) => Map (Day, DTime td) a -> td
      td' = const dTimeDescriptorInstance
      td = td' cmap
      time f = fromSomeDiscreteTime td $ f c
      isClassSlot (d, t) _ = classDay c == d
                          && dTimeWithin (time classBegins, time classEnds) t
      slots = Map.filterWithKey isClassSlot cmap
  slotsContents <- atomically . forM (Map.assocs slots)
                          $ \(k, TimeSlot var) -> (,) k <$> readTVar var
  let conflicts = Set.fromList . map (fromJust . snd)
                $ filter (isJust . snd) slotsContents
      writeClass = atomically . mapM_ writeSlot $ Map.elems slots
            where writeSlot s = timeSlotVar s `writeTVar` Just c
      result = if null conflicts then PutClassesSuccess
                                 else PutClassesConflict conflicts
  return (result, writeClass)

-----------------------------------------------------------------------------
