-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AUCSP.Agent.SharedSchedule where

import CSP.Coherence.Candidate
import AUCSP.Classes
import AUCSP.AgentsInterface.RoleData

import AgentSystem.Generic hiding (AgentOfRole)

import Data.Typeable

import Data.Set (Set)
import Data.Map (Map)

import Control.Concurrent.STM

-----------------------------------------------------------------------------

type AgentRef' = AgentRef ()

-- * Interface

class ScheduleInterface i where
  tryPutCandidate :: (HasCreator d AgentRef') =>
                     Candidate a d -> i -> IO (PutCandidateResult a d)

class HasCreator d c where getCreator :: d -> c

data PutCandidateResult a d = PutCandidateSuccess
                            | PutCandidateConflict (Set (Candidate a d))



newtype Schedule = Schedule (Set Class)
deriving instance (AgentOfRole Group, AgentOfRole Professor) => Show Schedule


-----------------------------------------------------------------------------
-- * Roles

-- | Interface
data SharedSchedule    = SharedSchedule   deriving (Show, Eq, Ord, Typeable)
-- | Internal
data ScheduleHolder    = ScheduleHolder   deriving (Show, Eq, Ord, Typeable)

data ScheduleObserver  = ScheduleObserver deriving (Show, Eq, Ord, Typeable)


-----------------------------------------------------------------------------
-- * States

-- | Hold references to internal 'ScheduleHolder's
newtype SharedScheduleState = SharedScheduleState (Map Classroom ScheduleHolderRef)
type    ScheduleHolderRef   = AgentRefOfRole ScheduleHolder


-- | Table Day-DiscreteTime with variable cells
data Timetable = forall td . DiscreteTimeDescriptor td =>
     Timetable Classroom (Map (Day, DTime td) (TimeSlot td))

newtype TimeSlot td = TimeSlot (TVar ClassCore)

-----------------------------------------------------------------------------

data ScheduleObserverState = ScheduleObserverState ScheduleCompleteness
                                                   SomeAgentSystem

newtype ScheduleCompleteness = ScheduleCompleteness
                              (Map (AgentRefOfRole Group) (TVar Bool))


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance RoleName SharedSchedule where roleName = show
instance AgentRole SharedSchedule where
  type RoleResult SharedSchedule = ()
  type RoleState  SharedSchedule = SharedScheduleState
  type RoleArgs   SharedSchedule = Map Classroom ScheduleHolderRef


instance RoleName ScheduleHolder where roleName = show
instance AgentRole ScheduleHolder where
  type RoleResult ScheduleHolder = ()
  type RoleState  ScheduleHolder = (Timetable, Classroom)
  type RoleArgs   ScheduleHolder = (SomeDiscreteTimeDescriptor, Classroom)

instance RoleName ScheduleObserver where roleName = show
instance AgentRole ScheduleObserver where
  type RoleResult ScheduleObserver = Schedule
  type RoleState  ScheduleObserver = ScheduleObserverState
  type RoleArgs   ScheduleObserver = (Set (AgentRefOfRole Group), SomeAgentSystem)

-----------------------------------------------------------------------------
